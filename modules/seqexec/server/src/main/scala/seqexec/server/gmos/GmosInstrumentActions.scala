// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats._
import cats.implicits._
import cats.data.NonEmptyList
import edu.gemini.spModel.config2.Config
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import seqexec.model.ActionType
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.NodAndShuffleStage._
import seqexec.model.enum.{Guiding, ObserveCommandResult}
import seqexec.engine.Action
import seqexec.engine.ParallelActions
import seqexec.engine.Result
import seqexec.server.Response
import seqexec.server.SeqTranslate._
import seqexec.server.StepType
import seqexec.server.FileIdAllocated
import seqexec.server.FileIdProvider
import seqexec.server.InstrumentActions
import seqexec.server.ObserveEnvironment
import seqexec.server.ObserveActions
import seqexec.server.ObserveContext
import seqexec.server.SeqexecFailure
import seqexec.server.ObserveActions._
import seqexec.server.gmos.GmosController.Config._
import seqexec.server.tcs.TcsController.{InstrumentOffset, OffsetP, OffsetQ}
import shapeless.tag
import squants.space.AngleConversions._

/**
  * Gmos needs different actions for N&S
  */
class GmosInstrumentActions[F[_]: MonadError[?[_], Throwable]: Logger, A <: GmosController.SiteDependentTypes](
  inst:   Gmos[F, A],
  config: Config
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
    dataId: String,
    env:    ObserveEnvironment[F]
  )(r:      ObserveCommandResult): F[Stream[F, Result[F]]] =
    r match {
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
          .emits[F, Result[F]](
            List(Result.Partial(NSStep), Result.OK(Response.Ignored))
          )
          .pure[F]
    }

  private def startObserve(
    fileId: ImageFileId,
    env:    ObserveEnvironment[F]
  ): F[Stream[F, Result[F]]] =
    // Essentially the same as default observation but with a custom tail
    for {
      (fileId, result) <- observePreamble(fileId, env)
      ret              <- observeTail(fileId, fileId, env)(result)
    } yield ret

  private def completeObserve(
    fileId: ImageFileId,
    env:    ObserveEnvironment[F]
  ): F[Stream[F, Result[F]]] =
    for {
      dataId  <- dataId(env)
      timeout <- inst.calcObserveTime(env.config)
      ret     <- inst.continueCommand(timeout)
      r       <- observeTail(fileId, dataId, env)(ret)
    } yield r

  private def observe(i: Int, env: ObserveEnvironment[F])(fileId: ImageFileId): Stream[F, Result[F]] =
    if (i === 0) {
      // The first steps allocates a file and runs the firt observe
      Stream.emit[F, Result[F]](Result.Partial(FileIdAllocated(fileId))) ++ Stream
        .eval(startObserve(fileId, env))
        .flatten
    } else if (i === Gmos.NsSequence.length - 1) {
      // the last step completes the observations with a complete and a tail
      Stream.eval(completeObserve(fileId, env).map(_ ++
        Stream.emit[F, Result[F]](Result.Partial(NSComplete))))
        .flatten
    } else {
      // Steps in betweet do a continue
      Stream
        .eval(
          inst
            .calcObserveTime(env.config)
            .flatMap(inst.continueCommand)
        )
        .as(Result.Partial(NSContinue)) ++
        Stream.emit(Result.OK(Response.Ignored))
    }

  // TODO reduce duplication with respect to InstrumentActions.safeObserve
  private def safeObserve(
    i: Int,
    env: ObserveEnvironment[F]
  ): Stream[F, Result[F]] = {
    Stream.eval(FileIdProvider.fileId(env).attempt).flatMap {
      case Right(fileId) =>
        observe(i, env)(fileId)
      case Left(e: SeqexecFailure) => Stream.emit(Result.Error(SeqexecFailure.explain(e)))
      case Left(e: Throwable) => Stream.emit(Result.Error(SeqexecFailure.explain(SeqexecFailure.SeqexecException(e))))
    }
  }

  /**
   * Calculate the actions for a N&S cycle
   */
  private def actionPositions(
    env:   ObserveEnvironment[F],
    rows:  Int,
    positions: Vector[NSPosition],
    post:  (Stream[F, Result[F]], ObserveEnvironment[F]) => Stream[F, Result[F]]
  ): List[ParallelActions[F]] =
    Gmos.NsSequence.zipWithIndex
      .map {
        case (stage, i) =>
          val rowsToShuffle = if (stage === StageA) 0 else rows
          val nsPositionO = positions.find(_.stage === stage)
          List(
            // Configure rows to shuffle
            NonEmptyList(
              inst
                .configureShuffle(rowsToShuffle)
                .as(Response.Configured(inst.resource))
                .toAction(ActionType.Configure(inst.resource)),
              (env.getTcs, nsPositionO).mapN{ case (tcs, nsPos) =>
                tcs.nod(
                  stage,
                  InstrumentOffset(
                    tag[OffsetP](nsPos.offset.p.toRadians.radians), tag[OffsetQ](nsPos.offset.q.toRadians.radians)),
                  nsPos.guide === Guiding.Guide
                ).toAction(ActionType.Configure(tcs.resource))
              }.toList
            ),
            // Do an obs observe/continue
            NonEmptyList.one(
              Action(
                ActionType.Observe,
                post( // post lets upstream to mix progress events
                  safeObserve(i, env),
                  env
                ),
                Action.State(Action.ActionState.Idle, Nil)
              )
            )
          )
      }
      .toList
      .flatten

  override def observeActions(
    env:  ObserveEnvironment[F],
    post: (Stream[F, Result[F]], ObserveEnvironment[F]) => Stream[F, Result[F]]
  ): List[ParallelActions[F]] =
    env.stepType match {
      case StepType.NodAndShuffle(i) if i === inst.resource =>
        Gmos
          .nsConfig(config)
          .map {
            case NSConfig.NoNodAndShuffle =>
              // This shouldn't happen but we need to code it anyway
              Nil
            case NSConfig.NodAndShuffle(cycles, rows, positions) =>
              // Initial notification of N&S Starting
              NonEmptyList.one(
                Action(ActionType.Undefined,
                       Stream.emits[F, Result[F]](
                         List(Result.Partial(NSStart),
                              Result.OK(Response.Ignored))
                       ),
                       Action.State(Action.ActionState.Idle, Nil))
              ) ::
                actionPositions(env, rows, positions, post) // Add steps for each cycle
                  .replicateA(cycles)
                  .toList
                  .flatten
          }
          .getOrElse(Nil)
      case _ =>
        // Regular GMOS obseravtions behave as any instrument
        InstrumentActions.defaultInstrumentActions[F].observeActions(env, post)
    }

  def runInitialAction(stepType: StepType): Boolean = true

}
