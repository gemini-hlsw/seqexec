// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.effect.{ContextShift, IO, Timer}
import cats.implicits._
import fs2.Stream
import io.prometheus.client.CollectorRegistry
import java.time.LocalDate
import java.util.UUID
import gem.Observation
import gem.enum.Site
import giapi.client.ghost.GhostClient
import giapi.client.gpi.GpiClient
import org.http4s.Uri
import org.http4s.Uri.uri
import seqexec.engine
import seqexec.engine.{Action, Result}
import seqexec.engine.Result.PauseContext
import seqexec.model.{ActionType, ClientId}
import seqexec.model.enum.{Instrument, Resource}
import seqexec.server.keywords.GdsClient
import seqexec.server.tcs.GuideConfig
import seqexec.server.tcs.GuideConfigDb
import shapeless.tag
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

object TestCommon {
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
    tag[GpiSettings][Uri](uri("http://localhost:8888/xmlrpc")),
    tag[GhostSettings][Uri](uri("http://localhost:8888/xmlrpc"))
  )

  def configureIO(resource: Resource): IO[Result[IO]] = IO.apply(Result.OK(Response.Configured(resource)))
  def pendingAction(resource: Resource): Action[IO] =
    engine.fromF[IO](ActionType.Configure(resource), configureIO(resource))
  def running(resource: Resource): Action[IO] = pendingAction(resource).copy(state = Action.State(
    Action.ActionState.Started, Nil))
  def done(resource: Resource): Action[IO] = pendingAction(resource).copy(state = Action.State(
    Action.ActionState.Completed(Response.Configured(resource)), Nil))
  val fileId = "fileId"
  def observing: Action[IO] = engine.fromF[IO](ActionType.Observe,
    IO.apply(Result.OK(Response.Observed(fileId)))).copy(state = Action.State(Action.ActionState.Started, Nil))
  def fileIdReady: Action[IO] = observing.copy(state = Action.State(Action.ActionState.Started,
    List(FileIdAllocated(fileId))))
  def observed: Action[IO] = observing.copy(state = Action.State(Action.ActionState.Completed(Response.Observed(fileId)),
    List(FileIdAllocated(fileId))))
  def paused: Action[IO] = observing.copy(state = Action.State(Action.ActionState.Paused(new PauseContext[IO]{}),
    List(FileIdAllocated(fileId))))
  def testCompleted(oid: Observation.Id)(st: EngineState): Boolean = st.sequences.get(oid)
    .exists(_.seq.status.isCompleted)
  private val sm = SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry()).unsafeRunSync
  private val guideDb = new GuideConfigDb[IO] {
    override def value: IO[GuideConfig] = GuideConfigDb.defaultGuideConfig.pure[IO]

    override def set(v: GuideConfig): IO[Unit] = IO.unit

    override def discrete: Stream[IO, GuideConfig] = Stream.emit(GuideConfigDb.defaultGuideConfig)
  }

  val gpiSim: GpiClient[IO] = GpiClient.simulatedGpiClient[IO].use(IO(_)).unsafeRunSync

  val ghostSim: GhostClient[IO] = GhostClient.simulatedGhostClient[IO].use(IO(_)).unsafeRunSync

  val seqexecEngine: SeqexecEngine = SeqexecEngine(GdsClient.alwaysOkClient, gpiSim, ghostSim, guideDb, defaultSettings, sm)
  def advanceOne(q: EventQueue[IO], s0: EngineState, put: IO[Either[SeqexecFailure, Unit]]): IO[Option[EngineState]] =
    (put *> seqexecEngine.stream(q.dequeue)(s0).take(1).compile.last).map(_.map(_._2))

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
    id,
    "",
    Instrument.F2,
    List(SequenceGen.PendingStepGen(1, Map(), Set.empty, SequenceGen.StepActionsGen(List(),
      Map(), _ => List(List(pendingAction(Instrument.F2)))
    )))
  )

  def sequenceNSteps(id: Observation.Id, n: Int): SequenceGen[IO] = SequenceGen[IO](
    id,
    "",
    Instrument.F2,
    List.range(1, n).map(SequenceGen.PendingStepGen(_, Map(), Set.empty, SequenceGen.StepActionsGen(List(),
      Map(), _ => List(List(pendingAction(Instrument.F2)))
    )))
  )

  def sequenceWithResources(id: Observation.Id, ins: Instrument, resources: Set[Resource]): SequenceGen[IO] = SequenceGen[IO](
    id,
    "",
    ins,
    List(
      SequenceGen.PendingStepGen(
        1, Map(), resources, SequenceGen.StepActionsGen(List(), resources.map(r => r ->pendingAction(r)).toMap,
          _ =>List()
        )
      ),
      SequenceGen.PendingStepGen(
        2, Map(), resources, SequenceGen.StepActionsGen(List(), resources.map(r => r ->pendingAction(r)).toMap,
          _ =>List()
        )
      )
    )
  )

}
