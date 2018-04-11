// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import java.time.LocalDate

import cats.data.Kleisli
import cats.effect.IO
import edu.gemini.seqexec.engine.{Action, Result, Sequence, Step}
import edu.gemini.seqexec.model.ActionType
import edu.gemini.seqexec.model.Model.Instrument.GmosS
import edu.gemini.seqexec.model.Model.{SequenceMetadata, SequenceState, StepConfig}
import edu.gemini.seqexec.server.SeqTranslate.ObserveContext
import edu.gemini.seqexec.server.flamingos2.Flamingos2ControllerSim
import edu.gemini.seqexec.server.gcal.GcalControllerEpics
import edu.gemini.seqexec.server.gmos.GmosControllerSim
import edu.gemini.seqexec.server.gnirs.GnirsControllerSim
import edu.gemini.seqexec.server.tcs.TcsControllerEpics
import edu.gemini.spModel.core.{Peer, Site}
import org.scalatest.FlatSpec
import squants.time.Seconds

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class SeqTranslateSpec extends FlatSpec {

  private val config: StepConfig = Map()
  private val fileId = "DummyFileId"
  private val seqId = "DummiSeqId"
  private def observeActions(state: Action.ActionState): List[Action] = List(Action(ActionType.Observe, Kleisli(v => IO(Result.OK(Result.Observed(fileId)))), Action.State(state, Nil)))
  private val s: Sequence.State = Sequence.State.status.set(SequenceState.Running.init)(Sequence.State.init(Sequence(
    seqId,
    SequenceMetadata(GmosS, None, ""),
    List(
      Step.init(
        1,
        None,
        config,
        Set(GmosS),
        List(observeActions(Action.Idle))
      )
    )
  ) ) )

  // Observe started
  private val s0: Sequence.State = s.start(0)
  // Observe pending
  private val s1: Sequence.State = s
  // Observe completed
  private val s2: Sequence.State = s.mark(0)(Result.OK(Result.Observed(fileId)))
  // Observe started, but with file Id already allocated
  private val s3: Sequence.State = s.start(0).mark(0)(Result.Partial(Result.FileIdAllocated(fileId), Kleisli(_ => IO(Result.OK(Result.Observed(fileId))))))
  // Observe paused
  private val s4: Sequence.State = s.mark(0)(Result.Paused(ObserveContext(_ => SeqAction(Result.OK(Result.Observed(fileId))), Seconds(1))))
  // Observe failed
  private val s5: Sequence.State = s.mark(0)(Result.Error("error"))

  private val systems = SeqTranslate.Systems(
    new ODBProxy(new Peer("localhost", 8443, null), ODBProxy.DummyOdbCommands),
    DhsClientSim(LocalDate.of(2016, 4, 15)),
    TcsControllerEpics,
    GcalControllerEpics,
    Flamingos2ControllerSim,
    GmosControllerSim.south,
    GmosControllerSim.north,
    GnirsControllerSim
  )

  private val translatorSettings = SeqTranslate.Settings(tcsKeywords = false, f2Keywords = false, gwsKeywords = false,
    gcalKeywords = false, gmosKeywords = false, gnirsKeywords = false)

  private val translator = SeqTranslate(Site.GS, systems, translatorSettings)

  "SeqTranslate" should "trigger stopObserve command only if exposure is in progress" in {
    assert(translator.stopObserve(seqId)(s0).isDefined)
    assert(translator.stopObserve(seqId)(s1).isEmpty)
    assert(translator.stopObserve(seqId)(s2).isEmpty)
    assert(translator.stopObserve(seqId)(s3).isDefined)
    assert(translator.stopObserve(seqId)(s4).isDefined)
    assert(translator.stopObserve(seqId)(s5).isEmpty)
  }

  "SeqTranslate" should "trigger abortObserve command only if exposure is in progress" in {
    assert(translator.abortObserve(seqId)(s0).isDefined)
    assert(translator.abortObserve(seqId)(s1).isEmpty)
    assert(translator.abortObserve(seqId)(s2).isEmpty)
    assert(translator.abortObserve(seqId)(s3).isDefined)
    assert(translator.abortObserve(seqId)(s4).isDefined)
    assert(translator.abortObserve(seqId)(s5).isEmpty)
  }

}
