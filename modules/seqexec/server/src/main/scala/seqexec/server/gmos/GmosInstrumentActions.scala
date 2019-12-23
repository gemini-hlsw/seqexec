// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.implicits._
import cats.effect.Concurrent
import cats.effect.Timer
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
import seqexec.server.gmos.NSObserveCommand._
import shapeless.tag
import squants.Time
import squants.space.AngleConversions._

/**
  * Gmos needs different actions for N&S
  */
class GmosInstrumentActions[F[_]: Concurrent: Timer: Logger, A <: GmosController.SiteDependentTypes](
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
    env:    ObserveEnvironment[F],
    nsCfg:  NSConfig.NodAndShuffle
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
                    (_: Time) => resumeObserve(fileId, env, nsCfg),
                    stopPausedObserve(fileId, dataId, env, nsCfg),
                    abortPausedObserve(fileId, dataId, env, nsCfg),
                    e
                  )
                )
          )
    }

  private def initialObserve(
    fileId: ImageFileId,
    env:    ObserveEnvironment[F],
    nsCfg:  NSConfig.NodAndShuffle,
    subExp: NSSubexposure,
    nsObsCmd: Ref[F, Option[NSObserveCommand]]
  ): F[Result[F]] =
    // Essentially the same as default observation but with a custom tail
    (for {
      (dataId, result) <- observePreamble(fileId, env)
      nsCmd            <- nsObsCmd.get
      ret              <- continueResult(fileId, dataId, env, nsCfg, subExp, nsCmd)(result)
    } yield ret).safeResult

  private def lastObserve(
    fileId: ImageFileId,
    env:    ObserveEnvironment[F],
    nsCfg:  NSConfig.NodAndShuffle
  ): F[Result[F]] =
    // the last step completes the observations doing an observeTail
    (for {
      dataId  <- dataId(env)
      timeout <- inst.calcObserveTime(env.config)
      ret     <- inst.controller.resumePaused(timeout)
      t       <- observeTail(fileId, dataId, env, nsCfg)(ret)
    } yield t).safeResult

  private def continueResult(
    fileId: ImageFileId,
    dataId: DataId,
    env:    ObserveEnvironment[F],
    nsCfg:  NSConfig.NodAndShuffle,
    subExp: NSSubexposure,
    nsObsCmd: Option[NSObserveCommand])(obsResult: ObserveCommandResult): F[Result[F]] =
    (nsObsCmd, obsResult) match {
      case (Some(PauseImmediately), ObserveCommandResult.Paused) |
           (_, ObserveCommandResult.Success) |
           (_, ObserveCommandResult.Aborted) |
           (_, ObserveCommandResult.Stopped)                     => observeTail(fileId, dataId, env, nsCfg)(obsResult)

      // Pause if this was the last subexposure of a cycle
      case (Some(PauseGracefully), ObserveCommandResult.Paused) if subExp.stageIndex === NsSequence.length - 1
                                                                 => observeTail(fileId, dataId, env, nsCfg)(obsResult)

      case (Some(StopImmediately), ObserveCommandResult.Paused)  => inst.controller.stopPaused
        .flatMap(observeTail(fileId, dataId, env, nsCfg))

      // Stop if this was the last subexposure of a cycle
      case (Some(StopGracefully), ObserveCommandResult.Paused)
        if subExp.stageIndex === NsSequence.length - 1
                                                                 => inst.controller.stopPaused
        .flatMap(observeTail(fileId, dataId, env, nsCfg))

      case (Some(AbortImmediately), ObserveCommandResult.Paused) => inst.controller.abortPaused
        .flatMap(observeTail(fileId, dataId, env, nsCfg))

      // Abort if this was the last subexposure of a cycle
      case (Some(AbortGracefully), ObserveCommandResult.Paused)
        if subExp.stageIndex === NsSequence.length - 1
                                                                 => inst.controller.abortPaused
        .flatMap(observeTail(fileId, dataId, env, nsCfg))

      // We reach here only if the result was Paused and no command made it stop/pause/abort
      case _                                                     => Result.Partial(NSContinue).pure[F].widen[Result[F]]

    }

  private def continueObserve(
    fileId: ImageFileId,
    env:    ObserveEnvironment[F],
    nsCfg:  NSConfig.NodAndShuffle,
    subExp: NSSubexposure,
    nsObsCmdRef: Ref[F, Option[NSObserveCommand]]
  ): F[Result[F]] = (
    for{
      t     <- inst.calcObserveTime(env.config)
      dId   <- dataId(env)
      r     <- inst.controller.resumePaused(t)
      nsCmd <- nsObsCmdRef.get
      x     <- continueResult(fileId, dId, env, nsCfg, subExp, nsCmd)(r)
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
    nsCfg:     NSConfig.NodAndShuffle,
    nsCmd:     Ref[F, Option[NSObserveCommand]]
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
      observationProgressStream(env)
        .mergeHaltR(
          Stream.emit(Result.Partial(NSSubexposureStart(sub))) ++
            (if (sub.firstSubexposure) {
               Stream.eval(initialObserve(fileId, env, nsCfg, sub, nsCmd))
            } else if (sub.lastSubexposure) {
               Stream.eval(lastObserve(fileId, env, nsCfg))
            } else {
               Stream.eval(continueObserve(fileId, env, nsCfg, sub, nsCmd))
            }) ++
            Stream.emit(Result.Partial(NSSubexposureEnd(sub)))
        ).handleErrorWith(catchObsErrors[F])
  }

  private def doObserve(
    fileId: ImageFileId,
    env:    ObserveEnvironment[F]
  ): Stream[F, Result[F]] =
    Gmos
      .nsConfig(config)
      .foldMap {
        case NSConfig.NoNodAndShuffle =>
          Stream.empty
        case c@NSConfig.NodAndShuffle(cycles, _, positions, _) =>
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

          // Clean NS command Ref
          Stream.eval(inst.nsCmdRef.set(none)) *>
          // Initial notification of N&S Starting
          Stream.emit(Result.Partial(NSStart(nsZero))) ++
            // each subexposure actions
            NSSubexposure
              .subexposures(cycles)
              .map {
                oneSubExposure(fileId, _, positions, env, c, inst.nsCmdRef)
              }
              .reduceOption(_ ++ _)
              .orEmpty ++
            Stream.emit(Result.Partial(NSComplete(nsLast))) ++
            Stream.emit(Result.OK(Response.Observed(fileId)))
      }

  def resumeObserve(
    fileId:  ImageFileId,
    env:     ObserveEnvironment[F],
    nsConfig: NSConfig.NodAndShuffle
  ): Stream[F, Result[F]] = {

      val nsLast =
        NSSubexposure
          .subexposures(nsConfig.cycles)
          .lastOption
          .getOrElse(NSSubexposure.Zero)

      Stream.eval(inst.nsCount).flatMap { cnt =>
        Stream.eval(inst.nsCmdRef.set(none)) *>
        NSSubexposure
          .subexposures(nsConfig.cycles)
          .map {
            oneSubExposure(fileId, _, nsConfig.positions, env, nsConfig, inst.nsCmdRef)
          }
          .drop(cnt)
          .reduceOption(_ ++ _)
          .orEmpty ++
          Stream.emit(Result.Partial(NSComplete(nsLast))) ++
          Stream.emit(Result.OK(Response.Observed(fileId)))
      }
  }

  def stopPausedObserve(
    fileId: ImageFileId,
    dataId: DataId,
    env:    ObserveEnvironment[F],
    nsCfg:  NSConfig.NodAndShuffle
  ): Stream[F, Result[F]] = Stream.eval(
    inst.controller.stopPaused.flatMap(observeTail(fileId, dataId, env, nsCfg))
  )

  def abortPausedObserve(
    fileId: ImageFileId,
    dataId: DataId,
    env:    ObserveEnvironment[F],
    nsCfg:  NSConfig.NodAndShuffle
  ): Stream[F, Result[F]] = Stream.eval(
    inst.controller.abortPaused.flatMap(observeTail(fileId, dataId, env, nsCfg))
  )

  def launchObserve(
    env: ObserveEnvironment[F]
  ): Stream[F, Result[F]] =
    Stream.eval(FileIdProvider.fileId(env)).flatMap { fileId =>
      Stream.emit(Result.Partial(FileIdAllocated(fileId))) ++ doObserve(fileId, env)
    }.handleErrorWith(catchObsErrors[F])

  override def observeActions(
    env: ObserveEnvironment[F]
  ): List[ParallelActions[F]] =
    env.stepType match {
      case StepType.NodAndShuffle(i) if i === inst.resource =>
        defaultObserveActions(launchObserve(env))
      case StepType.DarkOrBiasNS(i) if i === inst.resource  =>
        defaultObserveActions(launchObserve(env))

      case _ =>
        // Regular GMOS observations behave as any instrument
        defaultInstrumentActions[F].observeActions(env)
    }

  def runInitialAction(stepType: StepType): Boolean = true

}
