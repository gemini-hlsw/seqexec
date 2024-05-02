// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.{ Applicative, Monoid }
import cats.effect.IO
import cats.syntax.all._
import cats.data.NonEmptyList
import cats.effect.unsafe.IORuntime
import fs2.Stream
import org.typelevel.log4cats.noop.NoOpLogger
import org.typelevel.log4cats.Logger

import java.util.UUID
import edu.gemini.spModel.core.Peer
import seqexec.model.Observation
import lucuma.core.enums.Site
import giapi.client.ghost.GhostClient
import giapi.client.gpi.GpiClient
import org.http4s.Uri
import org.http4s.implicits._
import seqexec.engine
import seqexec.engine.{ Action, Result }
import seqexec.engine.Result.PauseContext
import seqexec.engine.Result.PartialVal
import seqexec.model.{ ActionType, ClientId }
import seqexec.model.enum.{ Instrument, Resource }
import seqexec.model.dhs._
import seqexec.model.SystemOverrides
import seqexec.model.config._
import seqexec.server.altair.{ AltairControllerSim, AltairKeywordReaderDummy }
import seqexec.server.flamingos2.Flamingos2ControllerSim
import seqexec.server.gcal.{ DummyGcalKeywordsReader, GcalControllerSim }
import seqexec.server.gems.{ GemsControllerSim, GemsKeywordReaderDummy }
import seqexec.server.ghost.GhostController
import seqexec.server.gmos.{ GmosControllerSim, GmosKeywordReaderDummy }
import seqexec.server.gnirs.{ GnirsControllerSim, GnirsKeywordReaderDummy }
import seqexec.server.gpi.GpiController
import seqexec.server.gsaoi.{ GsaoiControllerSim, GsaoiKeywordReaderDummy }
import seqexec.server.gws.DummyGwsKeywordsReader
import seqexec.server.keywords.{ DhsClient, DhsClientProvider, DhsClientSim }
import seqexec.server.nifs.{ NifsControllerSim, NifsKeywordReaderDummy }
import seqexec.server.niri.{ NiriControllerSim, NiriKeywordReaderDummy }
import seqexec.server.tcs.{
  DummyTcsKeywordsReader,
  GuideConfigDb,
  TcsNorthControllerSim,
  TcsSouthControllerSim
}
import org.scalatest.flatspec.AnyFlatSpec
import shapeless.tag

import scala.concurrent.duration._
import seqexec.server.igrins2.Igrins2Controller
import giapi.client.igrins2.Igrins2Client
import seqexec.server.keywords.GdsHttpClient
import seqexec.server.keywords.GdsXmlrpcClient

class TestCommon(implicit ioRuntime: IORuntime) extends AnyFlatSpec {
  import TestCommon._

  val defaultSystems: Systems[IO] =
    (DhsClientSim[IO].map(x =>
       new DhsClientProvider[IO] {
         override def dhsClient(instrumentName: String): DhsClient[IO] = x
       }
     ),
     Flamingos2ControllerSim[IO],
     GmosControllerSim.south[IO],
     GmosControllerSim.north[IO],
     GnirsControllerSim[IO](
       GdsHttpClient(GdsHttpClient.alwaysOkClient[IO], uri"http://localhost:8888/xmlrpc")
     ),
     GsaoiControllerSim[IO],
     gpiSim,
     ghostSim,
     igrins2Sim,
     NiriControllerSim[IO],
     NifsControllerSim[IO]
    ).mapN { (dhs, f2, gmosS, gmosN, gnirs, gsaoi, gpi, ghost, igrins2, niri, nifs) =>
      Systems[IO](
        OdbProxy(new Peer("localhost", 8443, null), new OdbProxy.DummyOdbCommands),
        dhs,
        TcsSouthControllerSim[IO],
        TcsNorthControllerSim[IO],
        GcalControllerSim[IO],
        f2,
        gmosS,
        gmosN,
        gnirs,
        gsaoi,
        gpi,
        ghost,
        igrins2,
        niri,
        nifs,
        AltairControllerSim[IO],
        GemsControllerSim[IO],
        GuideConfigDb.constant[IO],
        DummyTcsKeywordsReader[IO],
        DummyGcalKeywordsReader[IO],
        GmosKeywordReaderDummy[IO],
        GnirsKeywordReaderDummy[IO],
        NiriKeywordReaderDummy[IO],
        NifsKeywordReaderDummy[IO],
        GsaoiKeywordReaderDummy[IO],
        AltairKeywordReaderDummy[IO],
        GemsKeywordReaderDummy[IO],
        DummyGwsKeywordsReader[IO]
      )
    }.unsafeRunSync()

  val seqexecEngine: SeqexecEngine[IO] =
    SeqexecEngine.build(Site.GS, defaultSystems, defaultSettings).unsafeRunSync()

  def advanceOne(
    q:   EventQueue[IO],
    s0:  EngineState[IO],
    put: IO[Unit]
  ): IO[Option[EngineState[IO]]] =
    advanceN(q, s0, put, 1L)

  def advanceN(
    q:   EventQueue[IO],
    s0:  EngineState[IO],
    put: IO[Unit],
    n:   Long
  ): IO[Option[EngineState[IO]]] =
    (put *> seqexecEngine.stream(Stream.fromQueueUnterminated(q))(s0).take(n).compile.last)
      .map(_.map(_._2))

}

object TestCommon {

  implicit val logger: Logger[IO] = NoOpLogger.impl[IO]

  val defaultSettings: SeqexecEngineConfiguration = SeqexecEngineConfiguration(
    odb = uri"localhost",
    dhsServer = uri"http://localhost/",
    systemControl = SystemsControlConfiguration(
      altair = ControlStrategy.Simulated,
      gems = ControlStrategy.Simulated,
      dhs = ControlStrategy.Simulated,
      f2 = ControlStrategy.Simulated,
      gcal = ControlStrategy.Simulated,
      gmos = ControlStrategy.Simulated,
      gnirs = ControlStrategy.Simulated,
      gpi = ControlStrategy.Simulated,
      gpiGds = ControlStrategy.Simulated,
      ghost = ControlStrategy.Simulated,
      ghostGds = ControlStrategy.Simulated,
      igrins2 = ControlStrategy.Simulated,
      igrins2Gds = ControlStrategy.Simulated,
      gnirsGds = ControlStrategy.Simulated,
      gsaoi = ControlStrategy.Simulated,
      gws = ControlStrategy.Simulated,
      nifs = ControlStrategy.Simulated,
      niri = ControlStrategy.Simulated,
      tcs = ControlStrategy.Simulated
    ),
    odbNotifications = false,
    instForceError = false,
    failAt = 0,
    10.seconds,
    tag[GpiSettings][Uri](uri"vm://localhost:8888/gds-seqexec"),
    tag[GpiSettings][Uri](uri"http://localhost:8888/gds-seqexec"),
    tag[GhostSettings][Uri](uri"vm://localhost:8888/xmlrpc"),
    tag[GhostSettings][Uri](uri"http://localhost:8888/xmlrpc"),
    tag[Igrins2Settings][Uri](uri"vm://localhost:8888/xmlrpc"),
    tag[Igrins2Settings][Uri](uri"http://localhost:8888/xmlrpc"),
    tag[GnirsSettings][Uri](uri"http://localhost:8888/xmlrpc"),
    "",
    Some("127.0.0.1"),
    0,
    3.seconds,
    10.seconds,
    32
  )

  def configure[F[_]: Applicative](resource: Resource): F[Result[F]] =
    Result.OK(Response.Configured(resource)).pure[F].widen

  def pendingAction[F[_]: Applicative](resource: Resource): Action[F] =
    engine.fromF[F](ActionType.Configure(resource), configure(resource))

  def running[F[_]: Applicative](resource: Resource): Action[F] =
    Action.state[F].replace(Action.State(Action.ActionState.Started, Nil))(pendingAction(resource))

  def done[F[_]: Applicative](resource: Resource): Action[F] =
    Action
      .state[F]
      .replace(Action.State(Action.ActionState.Completed(Response.Configured(resource)), Nil))(
        pendingAction(resource)
      )

  private val fileId = toImageFileId("fileId")

  def observing[F[_]: Applicative]: Action[F] =
    Action
      .state[F]
      .replace(Action.State(Action.ActionState.Started, Nil))(
        engine.fromF[F](ActionType.Observe, Result.OK(Response.Observed(fileId)).pure[F].widen)
      )

  final case class PartialValue(s: String) extends PartialVal

  def observingPartial[F[_]: Applicative]: Action[F] =
    Action
      .state[F]
      .replace(Action.State(Action.ActionState.Started, Nil))(
        engine.fromF[F](ActionType.Observe,
                        Result.Partial(PartialValue("Value")).pure[F].widen,
                        Result.OK(Response.Ignored).pure[F].widen
        )
      )

  def fileIdReady[F[_]: Applicative]: Action[F] =
    Action
      .state[F]
      .replace(Action.State(Action.ActionState.Started, List(FileIdAllocated(fileId))))(observing)

  def observed[F[_]: Applicative]: Action[F] =
    Action
      .state[F]
      .replace(
        Action.State(Action.ActionState.Completed(Response.Observed(fileId)),
                     List(FileIdAllocated(fileId))
        )
      )(observing)

  def observePartial[F[_]: Applicative]: Action[F] =
    Action
      .state[F]
      .replace(Action.State(Action.ActionState.Started, List(FileIdAllocated(fileId))))(
        observingPartial
      )

  def paused[F[_]: Applicative]: Action[F] =
    Action
      .state[F]
      .replace(
        Action.State(Action.ActionState.Paused(new PauseContext[F] {}),
                     List(FileIdAllocated(fileId))
        )
      )(observing)

  def testCompleted(oid: Observation.Id)(st: EngineState[IO]): Boolean =
    st.sequences
      .get(oid)
      .exists(_.seq.status.isCompleted)

  private val gpiSim: IO[GpiController[IO]] = GpiClient
    .simulatedGpiClient[IO]
    .use(x =>
      IO(
        GpiController(x,
                      GdsHttpClient(GdsHttpClient.alwaysOkClient[IO], uri"http://localhost:8888")
        )
      )
    )

  private val ghostSim: IO[GhostController[IO]] = GhostClient
    .simulatedGhostClient[IO]
    .use(x =>
      IO(
        GhostController(
          x,
          GdsXmlrpcClient(GdsXmlrpcClient.alwaysOkClient[IO], uri"http://localhost:8888")
        )
      )
    )

  private val igrins2Sim: IO[Igrins2Controller[IO]] = Igrins2Client
    .simulatedIgrins2Client[IO]
    .use(x =>
      IO(
        Igrins2Controller(
          x,
          GdsHttpClient(GdsHttpClient.alwaysOkClient[IO], uri"http://localhost:8888/xmlrpc")
        )
      )
    )

  val seqId1: String            = "GS-2018B-Q-0-1"
  val seqObsId1: Observation.Id = Observation.Id.unsafeFromString(seqId1)
  val seqId2: String            = "GS-2018B-Q-0-2"
  val seqObsId2: Observation.Id = Observation.Id.unsafeFromString(seqId2)
  val seqId3: String            = "GS-2018B-Q-0-3"
  val seqObsId3: Observation.Id = Observation.Id.unsafeFromString(seqId3)
  val clientId: ClientId        = ClientId(UUID.randomUUID)

  def sequence(id: Observation.Id): SequenceGen[IO] = SequenceGen[IO](
    id = id,
    title = "",
    instrument = Instrument.F2,
    steps = List(
      SequenceGen.PendingStepGen(
        id = 1,
        Monoid.empty[DataId],
        config = CleanConfig.empty,
        resources = Set.empty,
        _ => InstrumentSystem.Uncontrollable,
        generator = SequenceGen.StepActionsGen(
          configs = Map(),
          post = (_, _) => List(NonEmptyList.one(pendingAction[IO](Instrument.F2))),
          None
        )
      )
    )
  )

  def sequenceNSteps(id: Observation.Id, n: Int): SequenceGen[IO] = SequenceGen[IO](
    id = id,
    title = "",
    instrument = Instrument.F2,
    steps = List
      .range(1, n)
      .map(
        SequenceGen.PendingStepGen(
          _,
          Monoid.empty[DataId],
          config = CleanConfig.empty,
          resources = Set.empty,
          _ => InstrumentSystem.Uncontrollable,
          generator = SequenceGen.StepActionsGen(
            configs = Map.empty,
            post = (_, _) => List(NonEmptyList.one(pendingAction[IO](Instrument.F2))),
            None
          )
        )
      )
  )

  def sequenceWithResources(
    id:        Observation.Id,
    ins:       Instrument,
    resources: Set[Resource]
  ): SequenceGen[IO] = SequenceGen[IO](
    id = id,
    title = "",
    instrument = ins,
    steps = List(
      SequenceGen.PendingStepGen(
        id = 1,
        Monoid.empty[DataId],
        config = CleanConfig.empty,
        resources = resources,
        _ => InstrumentSystem.Uncontrollable,
        generator = SequenceGen.StepActionsGen(
          configs = resources.map(r => r -> { _: SystemOverrides => pendingAction[IO](r) }).toMap,
          post = (_, _) => Nil,
          None
        )
      ),
      SequenceGen.PendingStepGen(
        id = 2,
        Monoid.empty[DataId],
        config = CleanConfig.empty,
        resources = resources,
        _ => InstrumentSystem.Uncontrollable,
        generator = SequenceGen.StepActionsGen(
          configs = resources.map(r => r -> { _: SystemOverrides => pendingAction[IO](r) }).toMap,
          post = (_, _) => Nil,
          None
        )
      )
    )
  )

}
