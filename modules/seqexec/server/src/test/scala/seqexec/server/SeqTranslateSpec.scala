// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import java.time.LocalDate

import cats.implicits._
import cats.effect._
import fs2.Stream
import giapi.client.Giapi
import giapi.client.gpi.GPIClient
import gem.Observation
import gem.enum.Site
import seqexec.engine.{Action, Result, Sequence}
import seqexec.model.enum.Instrument.GmosS
import seqexec.model.{ActionType, SequenceState, StepConfig}
import seqexec.server.SeqTranslate.ObserveContext
import seqexec.server.keywords.DhsClientSim
import seqexec.server.keywords.GDSClient
import seqexec.server.flamingos2.Flamingos2ControllerSim
import seqexec.server.gcal.GcalControllerSim
import seqexec.server.gmos.GmosControllerSim
import seqexec.server.gnirs.GnirsControllerSim
import seqexec.server.tcs.TcsControllerSim
import seqexec.server.gpi.GPIController
import edu.gemini.spModel.core.Peer
import giapi.client.ghost.GHOSTClient
import org.scalatest.FlatSpec
import org.http4s.Uri._
import squants.time.Seconds
import seqexec.server.Response.Observed
import seqexec.server.ghost.GHOSTController

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Throw"))
class SeqTranslateSpec extends FlatSpec {

  private val config: StepConfig = Map()
  private val fileId = "DummyFileId"
  private val seqId = Observation.Id.unsafeFromString("GS-2018A-Q-1-1")
  private def observeActions(state: Action.ActionState): List[Action[IO]] = List(
    Action(ActionType.Observe, Stream.emit(Result.OK(Observed(fileId))).covary[IO],
      Action.State(state, Nil))
  )
  private val seqg = SequenceGen(
    seqId,
    "",
    GmosS,
    List(SequenceGen.PendingStepGen(
      1,
      config,
      Set(GmosS),
      SequenceGen.StepActionsGen(List(), Map(), _ => List(observeActions(Action.Idle)))
    ))
  )

  private val baseState: EngineState = (SeqexecEngine.loadSequenceEndo(seqId, seqg) >>>
    (EngineState.sequenceStateIndex(seqId) ^|-> Sequence.State.status).set(
      SequenceState.Running.init))(EngineState.default)

  // Observe started
  private val s0: EngineState = EngineState.sequenceStateIndex(seqId)
    .modify(_.start(0))(baseState)
  // Observe pending
  private val s1: EngineState = baseState
  // Observe completed
  private val s2: EngineState = EngineState.sequenceStateIndex(seqId)
    .modify(_.mark(0)(Result.OK(Observed(fileId))))(baseState)
  // Observe started, but with file Id already allocated
  private val s3: EngineState = EngineState.sequenceStateIndex(seqId)
    .modify(_.start(0).mark(0)(Result.Partial(FileIdAllocated(fileId))))(baseState)
  // Observe paused
  private val s4: EngineState = EngineState.sequenceStateIndex(seqId)
    .modify(_.mark(0)(Result.Paused(ObserveContext(_ => SeqAction(Result.OK(Observed
    (fileId))), Seconds(1)))))(baseState)
  // Observe failed
  private val s5: EngineState = EngineState.sequenceStateIndex(seqId)
    .modify(_.mark(0)(Result.Error("error")))(baseState)

  private val systems = SeqTranslate.Systems(
    new ODBProxy(new Peer("localhost", 8443, null), ODBProxy.DummyOdbCommands),
    DhsClientSim(LocalDate.of(2016, 4, 15)),
    TcsControllerSim,
    GcalControllerSim,
    Flamingos2ControllerSim,
    GmosControllerSim.south,
    GmosControllerSim.north,
    GnirsControllerSim,
    GPIController(new GPIClient(Giapi.giapiConnectionIO(scala.concurrent.ExecutionContext.Implicits.global).connect.unsafeRunSync),
    new GDSClient(GDSClient.alwaysOkClient, uri("http://localhost:8888/xmlrpc"))),
    GHOSTController(new GHOSTClient[IO](Giapi.giapiConnectionIO(scala.concurrent.ExecutionContext.Implicits.global).connect.unsafeRunSync),
    new GDSClient(GDSClient.alwaysOkClient, uri("hhttp://localhost:8888/xmlrpc")))
  )

  private val translatorSettings = TranslateSettings(tcsKeywords = false, f2Keywords = false, gwsKeywords = false,
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
