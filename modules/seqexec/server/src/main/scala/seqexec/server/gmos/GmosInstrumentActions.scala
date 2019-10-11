// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats._
import cats.implicits._
import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import seqexec.model.dhs._
import seqexec.model.enum.NodAndShuffleStage._
import seqexec.model.enum.Guiding
import seqexec.model.enum.ObserveCommandResult
import seqexec.model.NSSubexposure
import seqexec.engine.ParallelActions
import seqexec.engine.Result
import seqexec.server._
import seqexec.server.InstrumentActions._
import seqexec.server.ObserveActions._
import seqexec.server.gmos.GmosController.Config._
import seqexec.server.gmos.NSPartial._
import seqexec.server.tcs.TcsController.InstrumentOffset
import seqexec.server.tcs.TcsController.OffsetP
import seqexec.server.tcs.TcsController.OffsetQ
import shapeless.tag
import squants.Time
import squants.space.AngleConversions._

/**
  * Gmos needs different actions for N&S
  */
class GmosInstrumentActions[F[_]: MonadError[?[_], Throwable]: Concurrent: Logger, A <: GmosController.SiteDependentTypes](
  inst:   Gmos[F, A],
  config: CleanConfig
) extends InstrumentActions[F] {
  override def observationProgressStream(
    env: ObserveEnvironment[F]
  ): Stream[F, Result[F]] =
    ObserveActions.observationProgressStream(env)

  // This tail is based on ObserveActions.observeTail
  // But it can understand how to process Partial observations
  // And can eventually return more than one result
  private def observeTail(
    fileId: ImageFileId,
    dataId: DataId,
    env:    ObserveEnvironment[F]
  )(r:      ObserveCommandResult): F[Result[F]] =
    r match {
      case ObserveCommandResult.Success =>
        okTail(fileId, dataId, stopped = false, env)
          .as(Result.Partial(NSFinalObs)) // For normally completed observations send a partial
      case ObserveCommandResult.Stopped =>
        okTail(fileId, dataId, stopped = true, env)
      case ObserveCommandResult.Aborted =>
        abortTail(env.systems, env.obsId, fileId)
      case ObserveCommandResult.Paused =>
        env.inst
          .calcObserveTime(env.config)
          .map(
            e =>
              Result
                .Paused(
                  ObserveContext(
                    (_: Time) => Stream.emit(Result.Error("Resuming a paused GMOS N&S observation is not yet supported")),
                    Stream.emit(Result.Error("Stopping a paused GMOS N&S observation is not yet supported")),
                    Stream.emit(Result.Error("Aborting a paused GMOS N&S observation is not yet supported")),
                    e
                  )
                )
          )
    }

  private def initialObserve(
    fileId: ImageFileId,
    env:    ObserveEnvironment[F]
  ): F[Result[F]] =
    // Essentially the same as default observation but with a custom tail
    (for {
      (dataId, result) <- observePreamble(fileId, env)
      ret              <- observeTail(fileId, dataId, env)(result)
    } yield ret).safeResult

  private def lastObserve(
    fileId: ImageFileId,
    env:    ObserveEnvironment[F]
  ): F[Result[F]] =
    // the last step completes the observations doing an observeTail
    (for {
      dataId  <- dataId(env)
      timeout <- inst.calcObserveTime(env.config)
      ret     <- inst.continueCommand(timeout)
      t       <- observeTail(fileId, dataId, env)(ret)
    } yield t).safeResult

  private def continueResult(
    fileId: ImageFileId,
    dataId: DataId,
    env:    ObserveEnvironment[F],
    subExp: NSSubexposure,
    nsObsCmd: Option[NSObserveCommand])(obsResult: ObserveCommandResult): F[Result[F]] =
    (nsObsCmd, obsResult) match {
      case (Some(PauseImmediately), ObserveCommandResult.Paused) |
           (_, ObserveCommandResult.Success) |
           (_, ObserveCommandResult.Aborted) |
           (_, ObserveCommandResult.Stopped) => observeTail(fileId, dataId, env)(obsResult)

      case (Some(PauseGracefully), ObserveCommandResult.Paused) if subExp.lastSubexposure
                                             => observeTail(fileId, dataId, env)(obsResult)

      case (Some(StopImmediately), _)        => inst.observeControl.stopPaused.self
        .flatMap(observeTail(fileId, dataId, env))

      case (Some(StopGracefully), _) if subExp.lastSubexposure
                                             => inst.observeControl.stopPaused.self
        .flatMap(observeTail(fileId, dataId, env))

      case (Some(AbortImmediately), _)       => inst.observeControl.abortPaused.self
        .flatMap(observeTail(fileId, dataId, env))

      case (Some(StopGracefully), _) if subExp.lastSubexposure
                                             => inst.observeControl.abortPaused.self
        .flatMap(observeTail(fileId, dataId, env))

      case _                                 => Result.Partial(NSContinue).pure[F].widen[Result[F]]

    }

  private def continueObserve(
    fileId: ImageFileId,
    env:    ObserveEnvironment[F],
    subExp: NSSubexposure,
    nsObsCmdRef: Ref[F, Option[NSObserveCommand]]
  ): F[Result[F]] = (
    for{
      t     <- inst.calcObserveTime(env.config)
      dId   <- dataId(env)
      r     <- inst.continueCommand(t)
      nsCmd <- nsObsCmdRef.get
      x     <- continueResult(fileId, dId, env, subExp, nsCmd)(r)
    } yield x
  ).safeResult

  /**
    * Stream of actions of one sub exposure
    */
  def oneSubExposure(
    fileId:    ImageFileId,
    sub:       NSSubexposure,
    positions: Vector[NSPosition],
    env:       ObserveEnvironment[F],
    nsCmd:     Ref[F, Option[NSObserveCommand]],
    post:      (Stream[F, Result[F]], ObserveEnvironment[F]) => Stream[F, Result[F]]
  ): Stream[F, Result[F]] = {
    val nsPositionO   = positions.find(_.stage === sub.stage)
    // TCS Nod
    (env.getTcs, nsPositionO).mapN {
      case (tcs, nsPos) =>
        Stream.emit(Result.Partial(NSTCSNodStart(sub))) ++
          Stream.eval(
            tcs
              .nod(
                sub.stage,
                InstrumentOffset(
                  tag[OffsetP](nsPos.offset.p.toRadians.radians),
                  tag[OffsetQ](nsPos.offset.q.toRadians.radians)
                ),
                nsPos.guide === Guiding.Guide
              )
              .as(Result.Partial(NSTCSNodComplete(sub)))
              .widen[Result[F]]
              .safeResult
          )
    }.orEmpty ++
    // Observes for each subexposure
    post(
      Stream.emit(Result.Partial(NSSubexposureStart(sub))) ++
        (if (sub.firstSubexposure) {
           Stream.eval(initialObserve(fileId, env))
         } else if (sub.lastSubexposure) {
           Stream.eval(lastObserve(fileId, env))
         } else {
           Stream.eval(continueObserve(fileId, env, sub, nsCmd))
         }) ++
        Stream.emit(Result.Partial(NSSubexposureEnd(sub))),
      env
    )
  }

  private def doObserve(
    fileId: ImageFileId,
    env:    ObserveEnvironment[F],
    post:   (Stream[F, Result[F]], ObserveEnvironment[F]) => Stream[F, Result[F]]
  ): Stream[F, Result[F]] =
    Gmos
      .nsConfig(config)
      .foldMap {
        case NSConfig.NoNodAndShuffle =>
          Stream.empty
        case NSConfig.NodAndShuffle(cycles, _, positions, _) =>
          val nsZero =
            NSSubexposure
              .subexposures(cycles)
              .headOption
              .getOrElse(NSSubexposure.Zero)
          val nsLast =
            NSSubexposure
              .subexposures(cycles)
              .lastOption
              .getOrElse(NSSubexposure.Zero)
          // Initial notification of N&S Starting
          Stream.emit(Result.Partial(NSStart(nsZero))) ++
            // each subexposure actions
            NSSubexposure
              .subexposures(cycles)
              .map {
                oneSubExposure(fileId, _, positions, env, inst.nsCmdRef, post)
              }
              .reduceOption(_ ++ _)
              .orEmpty ++
            Stream.emit(Result.Partial(NSComplete(nsLast))) ++
            Stream.emit(Result.OK(Response.Observed(fileId)))
      }

  def resumeObserve(
    fileId: ImageFileId,
    env:    ObserveEnvironment[F],
    nsConfig: NSConfig.NodAndShuffle,
    post:   (Stream[F, Result[F]], ObserveEnvironment[F]) => Stream[F, Result[F]]
    )(r: ObserveCommandResult): Stream[F, Result[F]] = {

      val nsLast =
        NSSubexposure
          .subexposures(nsConfig.cycles)
          .lastOption
          .getOrElse(NSSubexposure.Zero)


      Stream.eval(inst.nsCount).flatMap { cnt =>
        NSSubexposure
          .subexposures(nsConfig.cycles)
          .map {
            oneSubExposure(fileId, _, nsConfig.positions, env, inst.nsCmdRef, post)
          }
          .drop(cnt)
          .reduceOption(_ ++ _)
          .orEmpty ++
          Stream.emit(Result.Partial(NSComplete(nsLast))) ++
          Stream.emit(Result.OK(Response.Observed(fileId)))
      }
  }

  def launchObserve(
    env:  ObserveEnvironment[F],
    post: (Stream[F, Result[F]], ObserveEnvironment[F]) => Stream[F, Result[F]]
  ): Stream[F, Result[F]] =
    Stream.eval(FileIdProvider.fileId(env)).flatMap { fileId =>
      Stream.emit(Result.Partial(FileIdAllocated(fileId))) ++ doObserve(fileId, env, post)
    }.handleErrorWith(catchObsErrors[F])

  override def observeActions(
    env:  ObserveEnvironment[F],
    post: (Stream[F, Result[F]], ObserveEnvironment[F]) => Stream[F, Result[F]]
  ): List[ParallelActions[F]] =
    env.stepType match {
      case StepType.NodAndShuffle(i) if i === inst.resource =>
        defaultObserveActions(launchObserve(env, post))
      case StepType.DarkOrBiasNS(i) if i === inst.resource  =>
        defaultObserveActions(launchObserve(env, post))

      case _ =>
        // Regular GMOS observations behave as any instrument
        defaultInstrumentActions[F].observeActions(env, post)
    }

  def runInitialAction(stepType: StepType): Boolean = true

}
