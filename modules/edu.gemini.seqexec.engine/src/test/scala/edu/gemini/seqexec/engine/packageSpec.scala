package edu.gemini.seqexec.engine

import java.util.concurrent.Semaphore

import Event._
import org.scalatest.{FlatSpec, NonImplicitAssertions}
import edu.gemini.seqexec.model.Model.{Conditions, SequenceMetadata, SequenceState, StepConfig}
import edu.gemini.seqexec.model.Model.{F2, GmosS}
import edu.gemini.seqexec.model.UserDetails

import scala.concurrent.duration._
import scalaz._
import Scalaz._
import scalaz.Nondeterminism
import scalaz.concurrent.Task
import scalaz.stream.{Cause, Process, async}

class packageSpec extends FlatSpec with NonImplicitAssertions {

  /**
    * Emulates TCS configuration in the real world.
    *
    */
  val configureTcs: Action  = fromTask(for {
    _ <- Task(Thread.sleep(200))
  } yield Result.OK(Result.Configured("TCS")))

  /**
    * Emulates Instrument configuration in the real world.
    *
    */
  val configureInst: Action  = fromTask(for {
    _ <- Task(Thread.sleep(200))
  } yield Result.OK(Result.Configured("Instrument")))

  /**
    * Emulates an observation in the real world.
    *
    */
  val observe: Action  = fromTask(for {
    _ <- Task(Thread.sleep(200))
  } yield Result.OK(Result.Observed("DummyFileId")))

  val faulty: Action  = fromTask(for {
    _ <- Task(Thread.sleep(100))
  } yield Result.Error("There was an error in this action"))

  val config: StepConfig = Map()
  val seqId = "TEST-01"
  val qs1: Engine.State =
    Engine.State(
      Conditions.default,
      None,
      Map(
        (seqId,
         Sequence.State.init(
           Sequence(
             "First",
             SequenceMetadata(F2, None, ""),
             List(
               Step(
                 1,
                 None,
                 config,
                 Set(Resource.TCS, Resource.F2),
                 breakpoint = false,
                 skip = false,
                 List(
                   List(configureTcs.left, configureInst.left), // Execution
                   List(observe.left) // Execution
                 )
               ),
               Step(
                 2,
                 None,
                 config,
                 Set(Resource.TCS, Resource.OI, Resource.F2),
                 breakpoint = false,
                 skip = false,
                 List(
                   List(configureTcs.left, configureInst.left), // Execution
                   List(observe.left) // Execution
                 )
               )
             )
           )
         )
        )
      )
    )

  val seqG =
    Sequence.State.init(
      Sequence(
        "First",
        SequenceMetadata(GmosS, None, ""),
        List(
          Step(
            1,
            None,
            config,
            Set(Resource.GMOS),
            breakpoint = false,
            skip = false,
            List(
              List(configureTcs.left, configureInst.left), // Execution
              List(observe.left) // Execution
            )
          )
        )
      )
    )

  val seqId1 = seqId
  val seqId2 = "TEST-02"
  val seqId3 = "TEST-03"
  val qs2 = Engine.State(Conditions.default, None, qs1.sequences + (seqId2 -> qs1.sequences(seqId1)))
  val qs3 = Engine.State(Conditions.default, None, qs2.sequences + (seqId3 -> seqG))
  val user = UserDetails("telops", "Telops")

  def isFinished(status: SequenceState): Boolean = status match {
    case SequenceState.Idle      => true
    case SequenceState.Completed => true
    case SequenceState.Error(_)  => true
    case _                       => false
  }

  def runToCompletion(s0: Engine.State): Engine.State = {
    process(Process.eval(Task.now(start(seqId, user))))(s0).drop(1).takeThrough(
      a => !isFinished(a._2.sequences(seqId).status)
    ).runLast.unsafePerformSync.get._2
  }

  it should "be in Running status after starting" in {
    val p = Process.eval(Task.now(start(seqId, user)))
    val qs = process(p)(qs1).take(1).runLast.unsafePerformSync.get._2
    assert(qs.sequences(seqId).status === SequenceState.Running)
  }

  it should "be 0 pending executions after execution" in {
    val qs = runToCompletion(qs1)
    assert(qs.sequences(seqId).pending.isEmpty)
  }

  it should "be 2 Steps done after execution" in {
    val qs = runToCompletion(qs1)
    assert(qs.sequences(seqId).done.length == 2)
  }

  ignore should "Print execution" in {
    val p = Process.eval(Task(start(seqId, user)))
    intercept[Cause.Terminated](
      process(p)(qs1).run.unsafePerformSync
    )
  }

  ignore should "Print execution with pause" in {
    val p = Process.emitAll(List(start(seqId, user), pause(seqId, user), start(seqId, user))).evalMap(Task.now(_))
    intercept[Cause.Terminated](
       process(p)(qs1).run.unsafePerformSync
    )
  }

  it should "not run 2nd sequence because it's using the same resource" in {
    val p = Process.emitAll(List(start(seqId1, user), start(seqId2, user))).evalMap(Task.now(_))
    assert(
      process(p)(qs2).take(6).runLast.unsafePerformSync.get._2.sequences(seqId2).status === SequenceState.Idle
    )
  }

  it should "run 2nd sequence when there are no shared resources" in {
    val p = Process.emitAll(List(start(seqId1, user), start(seqId3, user))).evalMap(Task.now(_))

    assert(
      process(p)(qs3).take(6).runLast.unsafePerformSync.get._2.sequences(seqId3).status === SequenceState.Running
    )
  }

  "engine" should "keep processing input messages regardless of how long Actions take" in {
    val q = async.boundedQueue[Event](10)
    val startedFlag = new Semaphore(0)
    val finishFlag = new Semaphore(0)

    val qs = Engine.State(Conditions.default,
      None,
      Map((seqId, Sequence.State.init(Sequence(
        "First",
        SequenceMetadata(GmosS, None, ""),
        List(
          Step(
            1,
            None,
            config,
            Set(Resource.GMOS),
            breakpoint = false,
            skip = false,
            List(
              List(fromTask(Task.apply{
                startedFlag.release
                finishFlag.acquire
                Result.OK(Result.Configured("TCS"))
              }).left )
            )
          )
        )
      ) ) ) )
    )

    val result = Nondeterminism[Task].both(
      List(
        q.enqueueOne(start(seqId, user)),
        Task.apply(startedFlag.acquire),
        q.enqueueOne(Event.getState{_ => Task.delay{finishFlag.release} *> Task.delay(None)})
      ).sequenceU,
        process(q.dequeue)(qs).drop(1).takeThrough(a => !isFinished(a._2.sequences(seqId).status)).run
      ).timed(5.seconds).unsafePerformSyncAttempt
    assert(result.isRight)
  }

  "engine" should "not capture fatal errors." in {
    def s0(e: Error) = Engine.State(Conditions.default,
      None,
      Map((seqId, Sequence.State.init(Sequence(
        "First",
        SequenceMetadata(GmosS, None, ""),
        List(
          Step(
            1,
            None,
            config,
            Set(Resource.GMOS),
            breakpoint = false,
            skip = false,
            List(
              List(fromTask(Task.apply{
                throw e
              }).left )
            )
          )
        )
      ) ) ) )
    )

    intercept[OutOfMemoryError](
      runToCompletion(s0(new OutOfMemoryError))
    )
    intercept[StackOverflowError](
      runToCompletion(s0(new StackOverflowError))
    )
  }

  "engine" should "pass parameters to Actions." in {
    val p = Process.emitAll(List(Event.setOperator("John", user), Event.setObserver(seqId1, user, "Smith"), start(seqId1, user))).evalMap(Task.now(_))
    val s0 = Engine.State(Conditions.default,
      None,
      Map((seqId, Sequence.State.init(Sequence(
        "First",
        SequenceMetadata(GmosS, None, ""),
        List(
          Step(
            1,
            None,
            config,
            Set(Resource.GMOS),
            breakpoint = false,
            skip = false,
            List(
              List(new Action(v => Task(Result.OK(Result.Configured(v.operator.getOrElse("") + "-" + v.observer.getOrElse(""))))).left)
            )
          )
        )
      ) ) ) )
    )

    val sf = process(p)(s0).drop(3).takeThrough(
      a => !isFinished(a._2.sequences(seqId).status)
    ).runLast.unsafePerformSync.get._2

    assertResult(Result.OK(Result.Configured("John-Smith")))(sf.sequences.get(seqId).get.done.head.executions.head.head)
  }

}
