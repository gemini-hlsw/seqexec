// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Monoid
import cats.syntax.all._
import cats.effect._
import cats.effect.unsafe.implicits.global
import cats.data.NonEmptyList
import fs2.Stream
import seqexec.model.Observation
import lucuma.core.enums.Site
import seqexec.engine.{ Action, Result, Sequence }
import seqexec.model.Conditions
import seqexec.model.enum.Instrument.GmosS
import seqexec.model.dhs._
import seqexec.model.{ ActionType, SequenceState }
import seqexec.server.Response.Observed
import seqexec.server.TestCommon._
import squants.time.Seconds

class SeqTranslateSpec extends TestCommon {

  private val config: CleanConfig                                                     = CleanConfig.empty
  private val fileId                                                                  = "DummyFileId"
  private val seqId                                                                   = Observation.Id.unsafeFromString("GS-2018A-Q-1-1")
  private def observeActions(state: Action.ActionState[IO]): NonEmptyList[Action[IO]] =
    NonEmptyList.one(
      Action(ActionType.Observe,
             Stream.emit(Result.OK(Observed(toImageFileId(fileId)))).covary[IO],
             Action.State(state, Nil)
      )
    )

  private val seqg = SequenceGen(
    seqId,
    "",
    GmosS,
    List(
      SequenceGen.PendingStepGen(
        1,
        Monoid.empty[DataId],
        config,
        Set(GmosS),
        _ => InstrumentSystem.Uncontrollable,
        SequenceGen.StepActionsGen(Map.empty,
                                   (_, _) => List(observeActions(Action.ActionState.Idle))
        )
      )
    )
  )

  private val baseState: EngineState[IO] =
    (ODBSequencesLoader.loadSequenceEndo[IO](seqId, seqg, executeEngine) >>>
      EngineState
        .sequenceStateIndex[IO](seqId)
        .andThen(Sequence.State.status[IO])
        .replace(SequenceState.Running.init))(EngineState.default[IO])

  // Observe started
  private val s0: EngineState[IO] = EngineState
    .sequenceStateIndex[IO](seqId)
    .modify(_.start(0))(baseState)
  // Observe pending
  private val s1: EngineState[IO] = baseState
  // Observe completed
  private val s2: EngineState[IO] = EngineState
    .sequenceStateIndex[IO](seqId)
    .modify(_.mark(0)(Result.OK(Observed(toImageFileId(fileId)))))(baseState)
  // Observe started, but with file Id already allocated
  private val s3: EngineState[IO] = EngineState
    .sequenceStateIndex[IO](seqId)
    .modify(_.start(0).mark(0)(Result.Partial(FileIdAllocated(toImageFileId(fileId)))))(baseState)
  // Observe paused
  private val s4: EngineState[IO] = EngineState
    .sequenceStateIndex[IO](seqId)
    .modify(
      _.mark(0)(
        Result.Paused(
          ObserveContext[IO](
            _ => Stream.emit(Result.OK(Observed(toImageFileId(fileId)))).covary[IO],
            _ => Stream.empty,
            Stream.emit(Result.OK(Observed(toImageFileId(fileId)))).covary[IO],
            Stream.eval(SeqexecFailure.Aborted(seqId).raiseError[IO, Result[IO]]),
            Seconds(1)
          )
        )
      )
    )(baseState)
  // Observe failed
  private val s5: EngineState[IO] = EngineState
    .sequenceStateIndex[IO](seqId)
    .modify(_.mark(0)(Result.Error("error")))(baseState)
  // Observe aborted
  private val s6: EngineState[IO] = EngineState
    .sequenceStateIndex[IO](seqId)
    .modify(_.mark(0)(Result.OKAborted(Response.Aborted(toImageFileId(fileId)))))(baseState)

  private val translator = Ref
    .of[IO, Conditions](Conditions.Default)
    .flatMap(cs => SeqTranslate(Site.GS, defaultSystems, cs))
    .unsafeRunSync()

  "SeqTranslate" should "trigger stopObserve command only if exposure is in progress" in {
    assert(translator.stopObserve(seqId, graceful = false).apply(s0).isDefined)
    assert(translator.stopObserve(seqId, graceful = false).apply(s1).isEmpty)
    assert(translator.stopObserve(seqId, graceful = false).apply(s2).isEmpty)
    assert(translator.stopObserve(seqId, graceful = false).apply(s3).isDefined)
    assert(translator.stopObserve(seqId, graceful = false).apply(s4).isDefined)
    assert(translator.stopObserve(seqId, graceful = false).apply(s5).isEmpty)
    assert(translator.stopObserve(seqId, graceful = false).apply(s6).isEmpty)
  }

  "SeqTranslate" should "trigger abortObserve command only if exposure is in progress" in {
    assert(translator.abortObserve(seqId).apply(s0).isDefined)
    assert(translator.abortObserve(seqId).apply(s1).isEmpty)
    assert(translator.abortObserve(seqId).apply(s2).isEmpty)
    assert(translator.abortObserve(seqId).apply(s3).isDefined)
    assert(translator.abortObserve(seqId).apply(s4).isDefined)
    assert(translator.abortObserve(seqId).apply(s5).isEmpty)
    assert(translator.abortObserve(seqId).apply(s6).isEmpty)
  }

}
