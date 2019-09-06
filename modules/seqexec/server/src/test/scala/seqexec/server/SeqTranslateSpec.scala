// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.implicits._
import cats.effect._
import cats.data.NonEmptyList
import edu.gemini.spModel.core.Peer
import fs2.Stream
import giapi.client.gpi.GpiClient
import giapi.client.ghost.GhostClient
import gem.Observation
import gem.enum.Site
import io.chrisdavenport.log4cats.noop.NoOpLogger
import java.time.LocalDate
import org.scalatest.FlatSpec
import org.http4s.Uri._
import scala.concurrent.ExecutionContext
import seqexec.engine.{Action, Result, Sequence}
import seqexec.model.enum.Instrument.GmosS
import seqexec.model.dhs._
import seqexec.model.{ActionType, SequenceState, StepConfig}
import seqexec.server.keywords.DhsClientSim
import seqexec.server.keywords.GdsClient
import seqexec.server.flamingos2.Flamingos2ControllerSim
import seqexec.server.gcal.GcalControllerSim
import seqexec.server.gmos.GmosControllerSim
import seqexec.server.gnirs.GnirsControllerSim
import seqexec.server.gsaoi.GsaoiControllerSim
import seqexec.server.tcs.{GuideConfigDb, TcsNorthControllerSim, TcsSouthControllerSim}
import seqexec.server.gpi.GpiController
import seqexec.server.Response.Observed
import seqexec.server.ghost.GhostController
import seqexec.server.niri.NiriControllerSim
import seqexec.server.nifs.NifsControllerSim
import seqexec.server.altair.AltairControllerSim
import seqexec.server.gems.GemsControllerSim
import squants.time.Seconds

class SeqTranslateSpec extends FlatSpec {
  private implicit def unsafeLogger = NoOpLogger.impl[IO]

  implicit val ioTimer: Timer[IO] = IO.timer(ExecutionContext.global)
  implicit val csTimer: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  private val config: StepConfig = Map()
  private val fileId = "DummyFileId"
  private val seqId = Observation.Id.unsafeFromString("GS-2018A-Q-1-1")
  private def observeActions(state: Action.ActionState[IO]): NonEmptyList[Action[IO]] =
    NonEmptyList.one(
      Action(ActionType.Observe, Stream.emit(Result.OK(Observed(toImageFileId(fileId)))).covary[IO],
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
      SequenceGen.StepActionsGen(List(), Map(), _ => List(observeActions(Action.ActionState.Idle)))
    ))
  )

  private val baseState: EngineState = (ODBSequencesLoader.loadSequenceEndo(seqId, seqg) >>>
    (EngineState.sequenceStateIndex(seqId) ^|-> Sequence.State.status).set(
      SequenceState.Running.init))(EngineState.default)

  // Observe started
  private val s0: EngineState = EngineState.sequenceStateIndex(seqId)
    .modify(_.start(0))(baseState)
  // Observe pending
  private val s1: EngineState = baseState
  // Observe completed
  private val s2: EngineState = EngineState.sequenceStateIndex(seqId)
    .modify(_.mark(0)(Result.OK(Observed(toImageFileId(fileId)))))(baseState)
  // Observe started, but with file Id already allocated
  private val s3: EngineState = EngineState.sequenceStateIndex(seqId)
    .modify(_.start(0).mark(0)(Result.Partial(FileIdAllocated(toImageFileId(fileId)))))(baseState)
  // Observe paused
  private val s4: EngineState = EngineState.sequenceStateIndex(seqId)
    .modify(_.mark(0)(Result.Paused(ObserveContext[IO](_ =>
      Stream.emit(
        Result.OK(
          Observed(toImageFileId(fileId))
        )).covary[IO], Seconds(1)))))(baseState)
  // Observe failed
  private val s5: EngineState = EngineState.sequenceStateIndex(seqId)
    .modify(_.mark(0)(Result.Error("error")))(baseState)

  private val gpiSim: GpiController[IO] = GpiClient.simulatedGpiClient[IO].use(x => IO(GpiController(x,
    new GdsClient(GdsClient.alwaysOkClient, uri("http://localhost:8888/xmlrpc"))))
  ).unsafeRunSync
  private val ghostSim : GhostController[IO] = GhostClient.simulatedGhostClient[IO].use(x => IO(GhostController(x,
    new GdsClient(GdsClient.alwaysOkClient, uri("http://localhost:8888/xmlrpc"))))
  ).unsafeRunSync

  private val systems = Systems[IO](
    OdbProxy(new Peer("localhost", 8443, null), new OdbProxy.DummyOdbCommands),
    DhsClientSim.unsafeApply(LocalDate.of(2016, 4, 15)),
    TcsSouthControllerSim[IO],
    TcsNorthControllerSim[IO],
    GcalControllerSim[IO],
    Flamingos2ControllerSim.unsafeApply[IO],
    GmosControllerSim.unsafeSouth[IO],
    GmosControllerSim.unsafeNorth[IO],
    GnirsControllerSim.unsafeApply[IO],
    GsaoiControllerSim.unsafeApply[IO],
    gpiSim,
    ghostSim,
    NiriControllerSim.unsafeApply[IO],
    NifsControllerSim.unsafeApply[IO],
    AltairControllerSim,
    GemsControllerSim[IO],
    GuideConfigDb.constant[IO]
  )

  private val translatorSettings =
    TranslateSettings(tcsKeywords = false,
                f2Keywords        = false,
                gwsKeywords       = false,
                gcalKeywords      = false,
                gmosKeywords      = false,
                gnirsKeywords     = false,
                niriKeywords      = false,
                nifsKeywords      = false,
                altairKeywords    = false,
                gsaoiKeywords     = false,
                gemsKeywords      = false)

  private val translator = SeqTranslate(Site.GS, systems, translatorSettings)

  "SeqTranslate" should "trigger stopObserve command only if exposure is in progress" in {
    assert(translator.stopObserve(seqId).apply(s0).isDefined)
    assert(translator.stopObserve(seqId).apply(s1).isEmpty)
    assert(translator.stopObserve(seqId).apply(s2).isEmpty)
    assert(translator.stopObserve(seqId).apply(s3).isDefined)
    assert(translator.stopObserve(seqId).apply(s4).isDefined)
    assert(translator.stopObserve(seqId).apply(s5).isEmpty)
  }

  "SeqTranslate" should "trigger abortObserve command only if exposure is in progress" in {
    assert(translator.abortObserve(seqId).apply(s0).isDefined)
    assert(translator.abortObserve(seqId).apply(s1).isEmpty)
    assert(translator.abortObserve(seqId).apply(s2).isEmpty)
    assert(translator.abortObserve(seqId).apply(s3).isDefined)
    assert(translator.abortObserve(seqId).apply(s4).isDefined)
    assert(translator.abortObserve(seqId).apply(s5).isEmpty)
  }

}
