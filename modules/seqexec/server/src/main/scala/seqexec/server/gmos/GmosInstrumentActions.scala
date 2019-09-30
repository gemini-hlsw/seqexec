// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats._
import cats.implicits._
import cats.effect.Concurrent
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
import seqexec.server.tcs.TcsController.InstrumentOffset
import seqexec.server.tcs.TcsController.OffsetP
import seqexec.server.tcs.TcsController.OffsetQ
import shapeless.tag
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
                  ObserveContext(r => Stream.eval(observeTail(fileId, dataId, env)(r)), e)
                )
          )
      case ObserveCommandResult.Partial =>
        Result.Partial(NSStep).pure[F].widen[Result[F]]
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

  private def continueObserve(
    env: ObserveEnvironment[F]
  ): F[Result[F]] =
    // Steps in between do a continue
    inst
      .calcObserveTime(env.config)
      .flatMap(inst.continueCommand)
      .as(Result.Partial(NSContinue))
      .widen[Result[F]]
      .safeResult

  /**
    * Stream of actions of one sub exposure
    */
  def oneSubExposure(
    fileId:    ImageFileId,
    sub:       NSSubexposure,
    positions: Vector[NSPosition],
    env:       ObserveEnvironment[F],
    post:      (Stream[F, Result[F]], ObserveEnvironment[F]) => Stream[F, Result[F]]
  ): Stream[F, Result[F]] = {
    val nsPositionO   = positions.find(_.stage === sub.stage)
    // TCS Nod
    (env.getTcs, nsPositionO).mapN {
      case (tcs, nsPos) =>
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
            .as(Result.Partial(NSTCSNod))
            .widen[Result[F]]
            .safeResult
        )
    }.orEmpty ++
    // Observes for each subexposure
    (if (sub.firstSubexposure) {
       post(Stream.eval(initialObserve(fileId, env)), env)
     } else if (sub.lastSubexposure) {
       post(Stream.eval(lastObserve(fileId, env)), env)
     } else {
       post(Stream.eval(continueObserve(env)) ++ Stream .emit[F, Result[F]](Result.Partial(NSComplete)), env)
     })
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
          // Initial notification of N&S Starting
          Stream.emit[F, Result[F]](Result.Partial(NSStart)) ++
            // each subexposure actions
            NSSubexposure.subexposures(cycles)
              .map {
                oneSubExposure(fileId, _, positions, env, post)
              }
              .reduceOption(_ ++ _)
              .orEmpty
      }

  def launchObserve(
    env:  ObserveEnvironment[F],
    post: (Stream[F, Result[F]], ObserveEnvironment[F]) => Stream[F, Result[F]]
  ): Stream[F, Result[F]] =
    Stream.eval(FileIdProvider.fileId(env)).flatMap { fileId =>
      Stream.emit(Result.Partial(FileIdAllocated(fileId))) ++ doObserve(fileId, env, post)
    }

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
        // Regular GMOS obseravtions behave as any instrument
        defaultInstrumentActions[F].observeActions(env, post)
    }

  def runInitialAction(stepType: StepType): Boolean = true

}
