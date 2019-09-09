// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats._
import cats.implicits._
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import seqexec.model.dhs._
import seqexec.model.enum.NodAndShuffleStage
import seqexec.model.enum.NodAndShuffleStage._
import seqexec.model.enum.Guiding
import seqexec.model.enum.ObserveCommandResult
import seqexec.engine.ParallelActions
import seqexec.engine.Result
import seqexec.server.CleanConfig
import seqexec.server.FileIdAllocated
import seqexec.server.FileIdProvider
import seqexec.server.InstrumentActions
import seqexec.server.ObserveActions
import seqexec.server.ObserveContext
import seqexec.server.ObserveEnvironment
import seqexec.server.StepType
import seqexec.server.InstrumentActions._
import seqexec.server.ObserveActions._
import seqexec.server.gmos.GmosController.Config._
import seqexec.server.tcs.TcsController.InstrumentOffset
import seqexec.server.tcs.TcsController.OffsetP
import seqexec.server.tcs.TcsController.OffsetQ
import shapeless.tag
import squants.space.AngleConversions._

final case class Subexposure(
  totalCycles: Int,
  cycle:       Int,
  id:          Int,
  stage:       NodAndShuffleStage
) {
  val firstSubexposure = cycle === 0 && id === 0
  val lastSubexposure  = cycle === totalCycles - 1 && id === Gmos.NsSequence.length - 1
}

/**
  * Gmos needs different actions for N&S
  */
class GmosInstrumentActions[F[_]: MonadError[?[_], Throwable]: Logger, A <: GmosController.SiteDependentTypes](
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
  )(r:      ObserveCommandResult): Stream[F, Result[F]] =
    Stream
      .eval(r match {
        case ObserveCommandResult.Success =>
          okTail(fileId, dataId, stopped = false, env).map(Stream.emit)
        case ObserveCommandResult.Stopped =>
          okTail(fileId, dataId, stopped = true, env).map(Stream.emit)
        case ObserveCommandResult.Aborted =>
          abortTail(env.systems, env.obsId, fileId).map(Stream.emit)
        case ObserveCommandResult.Paused =>
          env.inst
            .calcObserveTime(env.config)
            .map(
              e =>
                Stream.emit(
                  Result
                    .Paused(ObserveContext(observeTail(fileId, dataId, env), e))
                )
            )
        case ObserveCommandResult.Partial =>
          Stream
            .emit[F, Result[F]](Result.Partial(NSStep))
            .pure[F]
      })
      .flatten

  private def initialObserve(
    fileId: ImageFileId,
    env:    ObserveEnvironment[F]
  ): Stream[F, Result[F]] =
    // Essentially the same as default observation but with a custom tail
    for {
      (dataId, result) <- Stream.eval(observePreamble(fileId, env))
      ret              <- observeTail(fileId, dataId, env)(result)
    } yield ret

  private def lastObserve(
    fileId: ImageFileId,
    env:    ObserveEnvironment[F]
  ): Stream[F, Result[F]] =
    // the last step completes the observations doing an observeTail
    Stream
      .eval(for {
        dataId  <- dataId(env)
        timeout <- inst.calcObserveTime(env.config)
        ret     <- inst.continueCommand(timeout)
      } yield observeTail(fileId, dataId, env)(ret))
      .flatten ++
      Stream.emit[F, Result[F]](Result.Partial(NSComplete))

  private def continueObserve(
    env: ObserveEnvironment[F]
  ): Stream[F, Result[F]] =
    // Steps in between do a continue
    Stream
      .eval(
        inst
          .calcObserveTime(env.config)
          .flatMap(inst.continueCommand)
      )
      .as(Result.Partial(NSContinue))

  // Calculate the subexposures
  private def subexposures(
    cycles: Int
  ): List[Subexposure] =
    (for {
      i     <- 0 until cycles
      j     <- 0 until Gmos.NsSequence.length
      stage = Gmos.NsSequence.toList.lift(j).getOrElse(StageA)
    } yield Subexposure(cycles, i, j, stage)).toList

  /**
    * Stream of actions of one sub exposure
    */
  def oneSubExposure(
    fileId:    ImageFileId,
    sub:       Subexposure,
    rows:      Int,
    positions: Vector[NSPosition],
    env:       ObserveEnvironment[F],
    post:      (Stream[F, Result[F]], ObserveEnvironment[F]) => Stream[F, Result[F]]
  ): Stream[F, Result[F]] = {
    val rowsToShuffle = if (sub.stage === StageA) 0 else rows
    val nsPositionO   = positions.find(_.stage === sub.stage)
    // Configure GMOS rows
    Stream.eval(
      inst.configureShuffle(rowsToShuffle).as(Result.Partial(NSRowsConfigure))
    ) ++
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
          )
      }.orEmpty ++
      // Observes for each subexposure
      (if (sub.firstSubexposure) {
         post(initialObserve(fileId, env), env)
       } else if (sub.lastSubexposure) {
         post(lastObserve(fileId, env), env)
       } else {
         post(continueObserve(env), env)
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
        case NSConfig.NodAndShuffle(cycles, rows, positions) =>
          // Initial notification of N&S Starting
          Stream.emit[F, Result[F]](Result.Partial(NSStart)) ++
            // each subexposure actions
            subexposures(cycles)
              .map {
                oneSubExposure(fileId, _, rows, positions, env, post)
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
