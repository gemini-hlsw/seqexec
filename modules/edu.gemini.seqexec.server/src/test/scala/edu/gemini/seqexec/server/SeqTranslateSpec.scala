// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import java.time.LocalDate

import edu.gemini.seqexec.engine.{Action, Result, Sequence, Step}
import edu.gemini.seqexec.model.ActionType
import edu.gemini.seqexec.model.Model.Instrument.GmosS
import edu.gemini.seqexec.model.Model.Resource.TCS
import edu.gemini.seqexec.model.Model.{Resource, SequenceMetadata, SequenceState, StepConfig}
import edu.gemini.seqexec.server.flamingos2.Flamingos2ControllerSim
import edu.gemini.seqexec.server.gcal.GcalControllerEpics
import edu.gemini.seqexec.server.gmos.GmosControllerSim
import edu.gemini.seqexec.server.tcs.TcsControllerEpics
import edu.gemini.spModel.core.{Peer, Site}
import org.scalatest.FlatSpec

import scalaz.Kleisli
import scalaz.concurrent.Task

/**
  * Created by jluhrs on 10/16/17.
  */
@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class SeqTranslateSpec extends FlatSpec {

  private val config: StepConfig = Map()
  private val s0: Sequence.State = Sequence.State.status.set(SequenceState.Running)(Sequence.State.init(Sequence(
      "First",
      SequenceMetadata(GmosS, None, ""),
      List(
        Step(
          1,
          None,
          config,
          Set(GmosS),
          breakpoint = false,
          skip = false,
          List(
            List(Action(ActionType.Observe, Kleisli(v => Task(Result.OK(Result.Observed("DummyFileId")))), Action.Idle))
          )
        )
      )
    ) ) )
  private val s1: Sequence.State = Sequence.State.status.set(SequenceState.Running)(Sequence.State.init(Sequence(
      "First",
      SequenceMetadata(GmosS, None, ""),
      List(
        Step(
          1,
          None,
          config,
          Set(GmosS),
          breakpoint = false,
          skip = false,
          List(
            List(Action(ActionType.Configure(TCS), Kleisli(v => Task(Result.OK(Result.Configured(Resource.TCS)))), Action.Idle))
          )
        )
      )
    ) ) )

  private val systems = SeqTranslate.Systems(
    new ODBProxy(new Peer("localhost", 8443, null), ODBProxy.DummyOdbCommands),
    DhsClientSim(LocalDate.of(2016, 4, 15)),
    TcsControllerEpics,
    GcalControllerEpics,
    Flamingos2ControllerSim,
    GmosControllerSim.south,
    GmosControllerSim.north
  )

  private val translatorSettings = SeqTranslate.Settings(tcsKeywords = false, f2Keywords = false, gwsKeywords = false, gcalKeywords = false, gmosKeywords = false)

  private val translator = SeqTranslate(Site.GS, systems, translatorSettings)

  "SeqTranslate" should "trigger stopObserve command only if exposure is in progress" in {
    assert(translator.stopObserve(s0).isDefined)
    assert(translator.stopObserve(s1).isEmpty)
  }

  "SeqTranslate" should "trigger abortObserve command only if exposure is in progress" in {
    assert(translator.abortObserve(s0).isDefined)
    assert(translator.abortObserve(s1).isEmpty)
  }

}
