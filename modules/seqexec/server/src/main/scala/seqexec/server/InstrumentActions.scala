// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.MonadError
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.effect.Timer
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import seqexec.model.ActionType
import seqexec.model.dhs.ImageFileId
import seqexec.engine.Action
import seqexec.engine.Action.ActionState
import seqexec.engine.ParallelActions
import seqexec.engine.Result

/**
  * Algebra to generate actions for an observation.
  * Most instruments behave the same but in some cases we need to customize
  * behavior. The two prime examples are:
  * GPI A&C
  * GMOS N&S
  * In both cases the InstrumentActions for the instrument can provide the correct behavior
  */
trait InstrumentActions[F[_]] {

  /**
    * Produce a progress stream for the given observe
    * @param env Properties of the observation
    */
  def observationProgressStream(
    env: ObserveEnvironment[F]
  ): Stream[F, Result[F]]

  /**
    * Builds a list of actions to run while observing
    * In most cases it is just a plain observe but could be skipped or made more complex
    * if needed. It should include the progress updates.
    * @param env Properties of the observation
    */
  def observeActions(
    env:  ObserveEnvironment[F]
  ): List[ParallelActions[F]]

  /**
    * Indicates if we should run the initial observe actions
    * e.g. requesting a file Id
    */
  def runInitialAction(stepType: StepType): Boolean
}

object InstrumentActions {

  /**
    * This is the default observe action, just a simple observe call
    */
  def defaultObserveActions[F[_]](
    observeResults: Stream[F, Result[F]]
  ): List[ParallelActions[F]] =
    List(
      NonEmptyList.one(
        Action(ActionType.Observe,
               observeResults,
               Action.State(ActionState.Idle, Nil))
      )
    )

  def launchObserve[F[_]: MonadError[?[_], Throwable]: Logger](
    env:       ObserveEnvironment[F],
    doObserve: (ImageFileId, ObserveEnvironment[F]) => Stream[F, Result[F]]
  ): Stream[F, Result[F]] =
    Stream.eval(FileIdProvider.fileId(env)).flatMap { fileId =>
      Stream.emit(Result.Partial(FileIdAllocated(fileId))) ++
        doObserve(fileId, env)
    }

  /**
    * Default Actions for most instruments it basically delegates to ObserveActions
    */
  def defaultInstrumentActions[F[_]: Concurrent: Timer: Logger]
    : InstrumentActions[F] =
    new InstrumentActions[F] {
      def observationProgressStream(
        env: ObserveEnvironment[F]
      ): Stream[F, Result[F]] =
        ObserveActions.observationProgressStream(env)

      override def observeActions(
        env: ObserveEnvironment[F]
      ): List[ParallelActions[F]] =
        defaultObserveActions(
          observationProgressStream(env)
            .mergeHaltR(launchObserve(env, ObserveActions.stdObserve[F]))
            .handleErrorWith(catchObsErrors[F])
        )

      def runInitialAction(stepType: StepType): Boolean = true
    }

}
