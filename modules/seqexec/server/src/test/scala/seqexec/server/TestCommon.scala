// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Applicative
import cats.effect.{ContextShift, IO, Timer}
import cats.implicits._
import cats.data.NonEmptyList
import io.prometheus.client.CollectorRegistry
import io.chrisdavenport.log4cats.noop.NoOpLogger
import java.time.LocalDate
import java.util.UUID
import gem.Observation
import gem.enum.Site
import org.http4s.Uri
import org.http4s.Uri.uri
import seqexec.engine
import seqexec.engine.{Action, Result}
import seqexec.engine.Result.PauseContext
import seqexec.engine.Result.PartialVal
import seqexec.model.{ActionType, ClientId}
import seqexec.model.enum.{Instrument, Resource}
import seqexec.model.dhs._
import seqexec.server.keywords.GdsClient
import shapeless.tag
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

object TestCommon {
  private implicit def logger = NoOpLogger.impl[IO]

  implicit val ioContextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  implicit val ioTimer: Timer[IO] =
    IO.timer(ExecutionContext.global)

  val defaultSettings: Settings = Settings(Site.GS,
    odbHost = "localhost",
    date = LocalDate.of(2017, 1, 1),
    dhsURI = uri("http://localhost/"),
    altairControl = ControlStrategy.Simulated,
    gemsControl = ControlStrategy.Simulated,
    dhsControl = ControlStrategy.Simulated,
    f2Control = ControlStrategy.Simulated,
    gcalControl = ControlStrategy.Simulated,
    ghostControl = ControlStrategy.Simulated,
    gmosControl = ControlStrategy.Simulated,
    gnirsControl = ControlStrategy.Simulated,
    gpiControl = ControlStrategy.Simulated,
    gpiGdsControl = ControlStrategy.Simulated,
    ghostGdsControl = ControlStrategy.Simulated,
    gsaoiControl = ControlStrategy.Simulated,
    gwsControl = ControlStrategy.Simulated,
    nifsControl = ControlStrategy.Simulated,
    niriControl = ControlStrategy.Simulated,
    tcsControl = ControlStrategy.Simulated,
    odbNotifications = false,
    instForceError = false,
    failAt = 0,
    10.seconds,
    tag[GpiSettings][Uri](uri("vm://localhost:8888/xmlrpc")),
    tag[GhostSettings][Uri](uri("vm://localhost:8888/xmlrpc")),
    tag[GpiSettings][Uri](uri("http://localhost:8888/xmlrpc")),
    tag[GhostSettings][Uri](uri("http://localhost:8888/xmlrpc"))
  )

  def configure[F[_]: Applicative](resource: Resource): F[Result[F]] =
    Result.OK(Response.Configured(resource)).pure[F].widen

  def pendingAction[F[_]: Applicative](resource: Resource): Action[F] =
    engine.fromF[F](ActionType.Configure(resource), configure(resource))

  def running[F[_]: Applicative](resource: Resource): Action[F] =
    Action.state.set(Action.State(Action.ActionState.Started, Nil))(pendingAction(resource))

  def done[F[_]: Applicative](resource: Resource): Action[F] =
    Action.state.set(
      Action.State(Action.ActionState.Completed(Response.Configured(resource)), Nil))(
        pendingAction(resource))

  val fileId = toImageFileId("fileId")

  def observing[F[_]: Applicative]: Action[F] =
    Action.state.set(
      Action.State(Action.ActionState.Started, Nil))(
        engine.fromF[F](
        ActionType.Observe,
            Result.OK(Response.Observed(fileId)).pure[F].widen))

  final case class PartialValue(s: String) extends PartialVal

  def observingPartial[F[_]: Applicative]: Action[F] =
    Action.state.set(
      Action.State(Action.ActionState.Started, Nil))(
        engine.fromF[F](
        ActionType.Observe,
            Result.Partial(PartialValue("Value")).pure[F].widen,
            Result.OK(Response.Ignored).pure[F].widen))

  def fileIdReady[F[_]: Applicative]: Action[F] =
    Action.state.set(
      Action.State(Action.ActionState.Started, List(FileIdAllocated(fileId))))(
        observing)

  def observed[F[_]: Applicative]: Action[F] =
    Action.state.set(
      Action.State(Action.ActionState.Completed(Response.Observed(fileId)), List(FileIdAllocated(fileId))))(
        observing)

  def observePartial[F[_]: Applicative]: Action[F] =
    Action.state.set(
      Action.State(Action.ActionState.Started, List(FileIdAllocated(fileId))))(
        observingPartial)

  def paused[F[_]: Applicative]: Action[F] =
    Action.state.set(
      Action.State(Action.ActionState.Paused(new PauseContext[F]{}), List(FileIdAllocated(fileId))))(
        observing)

  def testCompleted(oid: Observation.Id)(st: EngineState): Boolean = st.sequences.get(oid)
    .exists(_.seq.status.isCompleted)

  private val sm = SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry()).unsafeRunSync

  val seqexecEngine: SeqexecEngine = Systems.build(GdsClient.alwaysOkClient, defaultSettings).use(SeqexecEngine(_, defaultSettings, sm).pure[IO]).unsafeRunSync

  def advanceOne(q: EventQueue[IO], s0: EngineState, put: IO[Either[SeqexecFailure, Unit]]): IO[Option[EngineState]] =
    advanceN(q, s0, put, 1L)

  def advanceN(q: EventQueue[IO], s0: EngineState, put: IO[Either[SeqexecFailure, Unit]], n: Long): IO[Option[EngineState]] =
    (put *> seqexecEngine.stream(q.dequeue)(s0).take(n).compile.last).map(_.map(_._2))

  val seqId1: String = "GS-2018B-Q-0-1"
  val seqObsId1: Observation.Id = Observation.Id.unsafeFromString(seqId1)
  val seqId2: String = "GS-2018B-Q-0-2"
  val seqObsId2: Observation.Id = Observation.Id.unsafeFromString(seqId2)
  val seqId3: String = "GS-2018B-Q-0-3"
  val seqObsId3: Observation.Id = Observation.Id.unsafeFromString(seqId3)
  val clientId = ClientId(UUID.randomUUID)

  def sequence(id: Observation.Id): SequenceGen[IO] = SequenceGen[IO](
    id = id,
    title = "",
    instrument = Instrument.F2,
    steps = List(SequenceGen.PendingStepGen(
      id = 1,
      config = CleanConfig.empty,
      resources = Set.empty,
      generator = SequenceGen.StepActionsGen(
        pre = Nil,
        configs = Map(),
        post = _ => List(NonEmptyList.one(pendingAction[IO](Instrument.F2)))
    )))
  )

  def sequenceNSteps(id: Observation.Id, n: Int): SequenceGen[IO] = SequenceGen[IO](
    id = id,
    title = "",
    instrument = Instrument.F2,
    steps =
      List
        .range(1, n)
        .map(SequenceGen.PendingStepGen(
          _,
          config = CleanConfig.empty,
          resources = Set.empty,
          generator = SequenceGen.StepActionsGen(
            pre = Nil,
            configs = Map(),
            post = _ => List(NonEmptyList.one(pendingAction[IO](Instrument.F2)))
    )))
  )

  def sequenceWithResources(id: Observation.Id, ins: Instrument, resources: Set[Resource]): SequenceGen[IO] = SequenceGen[IO](
    id = id,
    title = "",
    instrument = ins,
    steps = List(
      SequenceGen.PendingStepGen(
        id = 1,
        config = CleanConfig.empty,
        resources = resources,
        generator = SequenceGen.StepActionsGen(
          pre = Nil,
          configs = resources.map(r => r -> pendingAction[IO](r)).toMap,
          post = _ => Nil
        )
      ),
      SequenceGen.PendingStepGen(
        id = 2,
        config = CleanConfig.empty,
        resources = resources,
        generator = SequenceGen.StepActionsGen(
          pre = Nil,
          configs = resources.map(r => r -> pendingAction[IO](r)).toMap,
          post = _ =>Nil
        )
      )
    )
  )

}
