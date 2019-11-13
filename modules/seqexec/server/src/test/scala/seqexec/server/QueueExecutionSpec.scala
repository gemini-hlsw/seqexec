// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.effect.IO
import cats.implicits._
import fs2.concurrent.Queue
import gem.Observation
import monocle.Monocle.index
import org.scalatest.NonImplicitAssertions
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inside.inside
import seqexec.engine.Sequence
import seqexec.model.{CalibrationQueueId, Observer, SequenceState, UserDetails}
import seqexec.model.BatchCommandState
import seqexec.model.enum.Instrument
import seqexec.model.enum.Resource.TCS
import seqexec.server.TestCommon._
import org.scalatest.flatspec.AnyFlatSpec

class QueueExecutionSpec extends AnyFlatSpec with Matchers with NonImplicitAssertions {

  "SeqexecEngine addSequenceToQueue" should
    "add sequence id to queue" in {
    val s0 = ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequence(seqObsId1), executeEngine).apply(EngineState.default[IO])

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.addSequenceToQueue(q, CalibrationQueueId, seqObsId1))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => exq.queue shouldBe List(seqObsId1)
      }
    }).unsafeRunSync
  }
  it should "not add sequence id if sequence does not exists" in {
    val badObsId = Observation.Id.unsafeFromString("NonExistent-1")
    val s0 = ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequence(seqObsId1), executeEngine).apply(EngineState.default[IO])

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.addSequenceToQueue(q, CalibrationQueueId, badObsId))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => assert(exq.queue.isEmpty)
      }
    }).unsafeRunSync
  }

  it should "not add sequence id if sequence is running or completed" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequence(seqObsId1), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequence(seqObsId2), executeEngine) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status)
        .set(SequenceState.Running.init) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId2) ^|-> Sequence.State.status)
        .set(SequenceState.Completed)
      )(EngineState.default)

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0,
        seqexecEngine.addSequenceToQueue(q, CalibrationQueueId, seqObsId1) *>
        seqexecEngine.addSequenceToQueue(q, CalibrationQueueId, seqObsId2),
        2)
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => assert(exq.queue.isEmpty)
      }
    }).unsafeRunSync
  }

  it should "not add sequence id if already in queue" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequence(seqObsId1), executeEngine) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).modify(_ :+ seqObsId1)
    )(EngineState.default)

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.addSequenceToQueue(q, CalibrationQueueId, seqObsId1))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => exq.queue shouldBe List(seqObsId1)
      }
    }).unsafeRunSync
  }

  "SeqexecEngine addSequencesToQueue" should
    "add sequence ids to queue" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequence(seqObsId1), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequence(seqObsId2), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId3, sequence(seqObsId3), executeEngine))(EngineState.default[IO])

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.addSequencesToQueue(q, CalibrationQueueId, List(seqObsId1, seqObsId2, seqObsId3)))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => exq.queue shouldBe List(seqObsId1, seqObsId2, seqObsId3)
      }
    }).unsafeRunSync
  }

  it should "not add sequence id if sequence is running or completed" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequence(seqObsId1), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequence(seqObsId2), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId3, sequence(seqObsId3), executeEngine) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status)
        .set(SequenceState.Running.init) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId2) ^|-> Sequence.State.status)
        .set(SequenceState.Completed)
      )(EngineState.default)

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0,
        seqexecEngine.addSequencesToQueue(q, CalibrationQueueId, List(seqObsId1, seqObsId2, seqObsId3)))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => exq.queue shouldBe List(seqObsId3)
      }
    }).unsafeRunSync
  }

  it should "not add sequence id if already in queue" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequence(seqObsId1), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequence(seqObsId2), executeEngine) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).modify(_ :+ seqObsId1)
    )(EngineState.default)

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.addSequencesToQueue(q, CalibrationQueueId, List(seqObsId1, seqObsId2)))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => exq.queue shouldBe List(seqObsId1, seqObsId2)
      }
    }).unsafeRunSync
  }

  "SeqexecEngine clearQueue" should
    "remove all sequences from queue" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequence(seqObsId1), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequence(seqObsId2), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId3, sequence(seqObsId3), executeEngine) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).modify(
        _ ++ List(seqObsId1, seqObsId2, seqObsId3)
      )
    )(EngineState.default)

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.clearQueue(q, CalibrationQueueId))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => exq.queue shouldBe List.empty
      }
    }).unsafeRunSync
  }

  "SeqexecEngine removeSequenceFromQueue" should
    "remove sequence id from queue" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequence(seqObsId1), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequence(seqObsId2), executeEngine) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).modify(
        _ ++ List(seqObsId1, seqObsId2)
      )
    )(EngineState.default)

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.removeSequenceFromQueue(q, CalibrationQueueId, seqObsId1))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => exq.queue shouldBe List(seqObsId2)
      }
    }).unsafeRunSync
  }

  it should "not remove sequence id if sequence is running" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequence(seqObsId1), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequence(seqObsId2), executeEngine) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).modify(
        _ ++ List(seqObsId1, seqObsId2)
      ) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.cmdState).set(
        BatchCommandState.Run(Observer(""), UserDetails("", ""), clientId)) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status).set(SequenceState
        .Running.init))(EngineState.default)

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.removeSequenceFromQueue(q, CalibrationQueueId, seqObsId1))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => exq.queue shouldBe List(seqObsId1, seqObsId2)
      }
    }).unsafeRunSync
  }

  "SeqexecEngine moveSequenceInQueue" should
    "move sequence id inside queue" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequence(seqObsId1), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequence(seqObsId2), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId3, sequence(seqObsId3), executeEngine) >>>
      (EngineState.queues[IO] ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).modify(
        _ ++ List(seqObsId1, seqObsId2, seqObsId3)
      )
    )(EngineState.default[IO])

    def testAdvance(obsId: Observation.Id, n: Int): Option[EngineState[IO]] =
      (for {
        q <- Queue.bounded[IO, executeEngine.EventType](10)
        r <- advanceOne(q, s0, seqexecEngine.moveSequenceInQueue(q, CalibrationQueueId, obsId, n, clientId))
      } yield r).unsafeRunSync

    val sf1 = testAdvance(seqObsId2, -1)

    inside(sf1.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
      case Some(exq) => exq.queue shouldBe List(seqObsId2, seqObsId1, seqObsId3)
    }

    val sf2 = testAdvance(seqObsId1, 2)

    inside(sf2.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
      case Some(exq) => exq.queue shouldBe List(seqObsId2, seqObsId3, seqObsId1)
    }

    val sf3 = testAdvance(seqObsId3, 4)

    inside(sf3.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
      case Some(exq) => exq.queue shouldBe List(seqObsId1, seqObsId2, seqObsId3)
    }

    val sf4 = testAdvance(seqObsId1, -2)

    inside(sf4.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
      case Some(exq) => exq.queue shouldBe List(seqObsId1, seqObsId2, seqObsId3)
    }
  }

  // A state with three sequences 1, 2 and 3. 1 and 3 use GMOS, 2 uses Flamingos2.
  // Calibration queue has the three sequences.
  private val alpha: EngineState[IO] = (
    ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequenceWithResources(seqObsId1, Instrument.F2,
      Set(Instrument.F2)), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequenceWithResources(seqObsId2, Instrument.GmosS,
        Set(Instrument.GmosS)), executeEngine) >>>
        ODBSequencesLoader.loadSequenceEndo[IO](seqObsId3, sequenceWithResources(seqObsId3, Instrument.F2,
          Set(Instrument.F2)), executeEngine) >>>
        (EngineState.queues[IO] ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).set(
          List(seqObsId1, seqObsId2, seqObsId3))
    )(EngineState.default[IO])

  "SeqexecEngine findRunnableObservations" should
    "return an empty set for an empty queue" in {
    val s0 = EngineState.default[IO]

    val r = SeqexecEngine.findRunnableObservations[IO](CalibrationQueueId)(s0)

    assert(r.isEmpty)
  }
  it should "return only the first observation that uses a common resource" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.F2, TCS)), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.GmosS, Set(Instrument.GmosS, TCS)), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId3, sequenceWithResources(seqObsId3,
        Instrument.Gsaoi, Set(Instrument.Gsaoi, TCS)), executeEngine) >>>
      (EngineState.queues[IO] ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).set(
        List(seqObsId1, seqObsId2, seqObsId3)))(EngineState.default)

    val r = SeqexecEngine.findRunnableObservations(CalibrationQueueId)(s0)

    r shouldBe Set(seqObsId1)
  }
  it should "return all observations with disjointed resource sets" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo(seqObsId1, sequenceWithResources(seqObsId1,
      Instrument.F2, Set(Instrument.F2)), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.GmosS, Set(Instrument.GmosS)), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId3, sequenceWithResources(seqObsId3,
        Instrument.Gsaoi, Set(Instrument.Gsaoi)), executeEngine) >>>
      (EngineState.queues[IO] ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).set(
        List(seqObsId1, seqObsId2, seqObsId3)))(EngineState.default)

    val r = SeqexecEngine.findRunnableObservations(CalibrationQueueId)(s0)

    r shouldBe Set(seqObsId1, seqObsId2, seqObsId3)
  }
  it should "not return observations with resources in use" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.F2, TCS)), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.GmosS, Set(Instrument.GmosS, TCS)), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId3, sequenceWithResources(seqObsId3,
        Instrument.F2, Set(Instrument.F2)), executeEngine) >>>
      (EngineState.queues[IO] ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).set(
        List(seqObsId2, seqObsId3)) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status).set(
        SequenceState.Running.init))(EngineState.default[IO])

    val r = SeqexecEngine.findRunnableObservations(CalibrationQueueId)(s0)

    assert(r.isEmpty)
  }

  "SeqexecEngine startQueue" should "run sequences in queue" in {
    // seqObsId1 and seqObsId2 can be run immediately, but seqObsId3 must be run after seqObsId1
    val s0 = alpha

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      _ <- seqexecEngine.startQueue(q, CalibrationQueueId, Observer(""), UserDetails("", ""), clientId)
      sf <- seqexecEngine.stream(q.dequeue)(s0).map(_._2)
        .takeThrough(_.sequences.values.exists(_.seq.status.isRunning)).compile.last
    } yield inside(sf) {
      case Some(s) => assert(testCompleted(seqObsId1)(s) && testCompleted(seqObsId2)(s) && testCompleted(seqObsId3)(s))
    } ).unsafeRunSync
  }

  it should "set observer for all sequences in queue" in {
    val observer = Observer("John Doe")
    val s0 = alpha

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      _ <- seqexecEngine.startQueue(q, CalibrationQueueId, observer, UserDetails("", ""), clientId)
      sf <- seqexecEngine.stream(q.dequeue)(s0).map(_._2)
        .takeThrough(_.sequences.values.exists(_.seq.status.isRunning)).compile.last
    } yield inside(sf.map(_.sequences)) {
      case Some(s) => assert(
          s.get(seqObsId1).map(_.observer) === Some(Some(observer)) &&
          s.get(seqObsId2).map(_.observer) === Some(Some(observer)) &&
          s.get(seqObsId3).map(_.observer) === Some(Some(observer))
        )
    } ).unsafeRunSync
  }

  it should "load the sequences to the corresponding instruments" in {
    // seqObsId1 and seqObsId2 can be run immediately, but seqObsId3 must be run after seqObsId1
    val s0 = alpha

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      s1 <- advanceOne(q, s0,
        seqexecEngine.startQueue(q, CalibrationQueueId, Observer(""), UserDetails("", ""), clientId))
    } yield inside(s1.map(_.selected)) {
      case Some(sel1) => assert(sel1.get(Instrument.F2) === Some(seqObsId1) &&
                                sel1.get(Instrument.GmosS) === Some(seqObsId2))
    } ).unsafeRunSync
  }

  "SeqexecEngine stopQueue" should "stop running the sequences in the queue" in {
    val s0 = alpha

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      _ <- seqexecEngine.startQueue(q, CalibrationQueueId, Observer(""), UserDetails("", ""), clientId)
      _ <- seqexecEngine.stopQueue(q, CalibrationQueueId, clientId)
      sf <- seqexecEngine.stream(q.dequeue)(s0).map(_._2)
        .takeThrough(_.sequences.values.exists(_.seq.status.isRunning)).compile.last
    } yield inside(sf) {
      case Some(s) => assert(!testCompleted(seqObsId1)(s))
                      assert(!testCompleted(seqObsId2)(s))
                      assert(!testCompleted(seqObsId3)(s))
    } ).unsafeRunSync
  }

  "SeqexecEngine start sequence" should "not run sequence not in queue if running queue needs the same resources" in {
    val s0 = (EngineState.queues ^|-? index(CalibrationQueueId)).modify(x => x.copy(cmdState =
        BatchCommandState.Run(Observer(""), UserDetails("", ""), clientId), queue = List(seqObsId1,
        seqObsId2)))(alpha)

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.start(q, seqObsId3, UserDetails("", ""),clientId))
    } yield inside(sf.flatMap(_.sequences.get(seqObsId3))) {
      case Some(s) => assert(s.seq.status === SequenceState.Idle)
    } ).unsafeRunSync
  }

  it should "run sequence if it is in running queue and resources are available" in {
    val s0 = (EngineState.queues ^|-? index(CalibrationQueueId)).modify(x => x.copy(cmdState =
      BatchCommandState.Run(Observer(""), UserDetails("", ""), clientId)))(alpha)

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.start(q, seqObsId3, UserDetails("", ""),clientId), 2)
    } yield inside(sf.flatMap(_.sequences.get(seqObsId3))) {
      case Some(s) => assert(s.seq.status.isRunning)
    } ).unsafeRunSync
  }

  it should "not run sequence in running queue if resources are not available" in {
    // Calibration queue with three sequences, 1, 2, and 3. 1 and 3 use the same instrument, different from 2.
    // Sequence 1 is running
    val s0 = ((EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status).set(SequenceState.Running.init) >>>
      (EngineState.queues[IO] ^|-? index(CalibrationQueueId)).modify(x => x.copy(cmdState =
        BatchCommandState.Run(Observer(""), UserDetails("", ""), clientId))
      )
    )(alpha)

    // Attempt to run sequence 3 must fail
    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.start(q, seqObsId3, UserDetails("", ""),clientId))
    } yield inside(sf.flatMap(_.sequences.get(seqObsId3))) {
      case Some(s) => assert(s.seq.status === SequenceState.Idle)
    } ).unsafeRunSync
  }

  "SeqexecEngine" should "not automatically schedule queued sequences stopped by the user" in {
    // Calibration queue with three sequences, 1, 2, and 3. 1 and 3 use the same instrument, different from 2, but
    // none is running (i.e. the user stopped 1).
    val s0 =(EngineState.queues[IO] ^|-? index(CalibrationQueueId)).modify(x => x.copy(cmdState =
        BatchCommandState.Run(Observer(""), UserDetails("", ""), clientId),
          queue = List(seqObsId1, seqObsId3)
      )
    )(alpha)

    // Sequence 2 is started. SeqexecEngine must not schedule 1 nor 3 when 2 completes.
    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      _ <- seqexecEngine.start(q, seqObsId2, UserDetails("", ""), clientId)
      sf <- seqexecEngine.stream(q.dequeue)(s0).map(_._2).drop(1)
        .takeThrough(_.sequences.values.exists(_.seq.status.isRunning)).compile.last
    } yield inside(sf) {
      case Some(s) => assert(!testCompleted(seqObsId1)(s))
                      assert(testCompleted(seqObsId2)(s))
                      assert(!testCompleted(seqObsId3)(s))
    } ).unsafeRunSync
  }

  it should "not automatically schedule queued sequences that ended in error" in {
    // Calibration queue with three sequences, 1, 2, and 3. 1 and 3 use the same instrument, different from 2, but
    // none is running (i.e. the user stopped 1).
    val s0 = ((EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status).set(SequenceState.Failed("")) >>>
      (EngineState.queues[IO] ^|-? index(CalibrationQueueId)).modify(x => x.copy(cmdState =
        BatchCommandState.Run(Observer(""), UserDetails("", ""), clientId)
      ))
    )(alpha)

    // Sequence 2 is started. SeqexecEngine must not schedule 1 nor 3 when 2 completes.
    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      _ <- seqexecEngine.start(q, seqObsId2, UserDetails("", ""), clientId)
      sf <- seqexecEngine.stream(q.dequeue)(s0).map(_._2).drop(1)
        .takeThrough(_.sequences.values.exists(_.seq.status.isRunning)).compile.last
    } yield inside(sf) {
      case Some(s) => assert(!testCompleted(seqObsId1)(s))
        assert(testCompleted(seqObsId2)(s))
        assert(!testCompleted(seqObsId3)(s))
    } ).unsafeRunSync
  }

  it should "allow restarting a queued sequence stopped by the user" in {
    // State has three sequences, 1, 2 and 3. 1 and 3 use the same instrument, different from 2
    // Queue sequences 1 and 3. Queue is running, but sequence 1 is not (i.e. the user stopped 1).
    val s0 = (EngineState.queues ^|-? index(CalibrationQueueId)).modify(x => x.copy(cmdState =
        BatchCommandState.Run(Observer(""), UserDetails("", ""), clientId), queue = List(seqObsId1, seqObsId3)
    ))(alpha)

    // Sequence 1 is started. It should run. And when finishes, sequence 3 should be run too.
    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      _ <- seqexecEngine.start(q, seqObsId1, UserDetails("", ""), clientId)
      sf <- seqexecEngine.stream(q.dequeue)(s0).map(_._2).drop(1)
        .takeThrough(_.sequences.values.exists(_.seq.status.isRunning)).compile.last
    } yield inside(sf) {
      case Some(s) => assert(testCompleted(seqObsId1)(s))
                      assert(testCompleted(seqObsId3)(s))
    } ).unsafeRunSync
  }

  it should "allow restarting a queued sequence that ended in error" in {
    // State has three sequences, 1, 2 and 3. 1 and 3 use the same instrument, different from 2
    // Queue has sequences 1 and 3. Queue is running, but sequence 1 is in error state.
    val s0 = ((EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status).set(SequenceState.Failed("")) >>>
      (EngineState.queues[IO] ^|-? index(CalibrationQueueId)).modify(x => x.copy(cmdState =
        BatchCommandState.Run(Observer(""), UserDetails("", ""), clientId), queue = List(seqObsId1, seqObsId3)
      ))
    )(alpha)

    // Sequence 2 is started. SeqexecEngine must not schedule 1 nor 3 when 2 completes.
    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      _ <- seqexecEngine.start(q, seqObsId1, UserDetails("", ""), clientId)
      sf <- seqexecEngine.stream(q.dequeue)(s0).map(_._2).drop(1)
        .takeThrough(_.sequences.values.exists(_.seq.status.isRunning)).compile.last
    } yield inside(sf) {
      case Some(s) => assert(testCompleted(seqObsId1)(s))
                      assert(testCompleted(seqObsId3)(s))
    } ).unsafeRunSync
  }

  it should "not allow starting a non queued sequence if it uses resources required by a running queue" in {
    // State has three sequences, 1, 2 and 3. 1 and 3 use the same instrument, different from 2
    // Queue only has sequence 1. Queue is running, but sequence 1 is not.
    val s0 = (EngineState.queues[IO] ^|-? index(CalibrationQueueId)).modify(x => x.copy(cmdState =
        BatchCommandState.Run(Observer(""), UserDetails("", ""), clientId), queue = List(seqObsId1)
    ))(alpha)

    // Attempt to run sequence 3. it should fail, because it uses the same instrument as sequence 1
    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      _ <- seqexecEngine.start(q, seqObsId3, UserDetails("", ""), clientId)
      sf <- seqexecEngine.stream(q.dequeue)(s0).map(_._2)
        .takeThrough(_.sequences.values.exists(_.seq.status.isRunning)).compile.last
    } yield inside(sf) {
      case Some(s) => assert(!testCompleted(seqObsId3)(s))
    } ).unsafeRunSync
  }

  "SeqexecEngine addSequenceToQueue" should "start added sequence if queue is running and resources are available" in {
    // State has three sequences, 1, 2 and 3. 1 and 3 use the same instrument, different from 2
    // Queue only has sequence 2. Queue is running, but sequence 2 is not.
    val s0 = (EngineState.queues ^|-? index(CalibrationQueueId)).modify(x => x.copy(cmdState =
        BatchCommandState.Run(Observer(""), UserDetails("", ""), clientId), queue = List(seqObsId2)
    ))(alpha)

    // Sequence 1 is added. It uses a different instrument, so it should be started
    // Sequence 2 should not be started
    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      _  <- seqexecEngine.addSequenceToQueue(q, CalibrationQueueId, seqObsId1)
      sf <- seqexecEngine.stream(q.dequeue)(s0).map(_._2)
        .takeThrough(_.sequences.values.exists(_.seq.status.isRunning)).compile.last
    } yield inside(sf) {
      case Some(s) => assert(testCompleted(seqObsId1)(s))
                      assert(!testCompleted(seqObsId2)(s))
    } ).unsafeRunSync
  }

  it should "not start added sequence if queue is running but resources are unavailable" in {
    // State has three sequences, 1, 2 and 3. 1 and 3 use the same instrument, different from 2
    // Queue only has sequence 3. Queue is running, but sequence 3 is not.
    val s0 = (EngineState.queues ^|-? index(CalibrationQueueId)).modify(x => x.copy(cmdState =
        BatchCommandState.Run(Observer(""), UserDetails("", ""), clientId), queue = List(seqObsId3)
    ))(alpha)

    // Sequence 1 is added to the queue. Because it uses the same instrument as 3, it should not be started.
    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      _  <- seqexecEngine.addSequenceToQueue(q, CalibrationQueueId, seqObsId1)
      sf <- seqexecEngine.stream(q.dequeue)(s0).map(_._2)
        .takeThrough(_.sequences.values.exists(_.seq.status.isRunning)).compile.last
    } yield inside(sf) {
      case Some(s) => assert(!testCompleted(seqObsId1)(s))
                      assert(!testCompleted(seqObsId3)(s))
    } ).unsafeRunSync
  }

  "SeqexecEngine removeSequenceFromQueue" should "start sequences waiting resources freed by removed sequence" in {
    // State has three sequences, 1, 2 and 3. 1 and 3 use the same instrument, different from 2. 1 and 2 use TCS
    // Queue has all three sequences. Queue is running, but no sequence is running (sequence 1 was stopped by user)
    val s0 = (
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequenceWithResources(seqObsId1, Instrument.F2,
        Set(Instrument.F2, TCS)), executeEngine) >>>
        ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequenceWithResources(seqObsId2, Instrument.GmosS,
          Set(Instrument.GmosS, TCS)), executeEngine) >>>
        ODBSequencesLoader.loadSequenceEndo[IO](seqObsId3, sequenceWithResources(seqObsId3, Instrument.F2,
          Set(Instrument.F2)), executeEngine) >>>
        (EngineState.queues ^|-? index(CalibrationQueueId)).modify(x => x.copy(cmdState =
          BatchCommandState.Run(Observer(""), UserDetails("", ""), clientId), queue = List(seqObsId1, seqObsId2,
            seqObsId3)
        ))
    )(EngineState.default)

    // Sequence 1 is removed. Because it was holding sequence 2 and 3, they can now start
    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      _  <- seqexecEngine.removeSequenceFromQueue(q, CalibrationQueueId, seqObsId1)
      sf <- seqexecEngine.stream(q.dequeue)(s0).map(_._2)
        .takeThrough(_.sequences.values.exists(_.seq.status.isRunning)).compile.last
    } yield inside(sf) {
      case Some(s) => assert(!testCompleted(seqObsId1)(s))
                      assert(testCompleted(seqObsId2)(s))
                      assert(testCompleted(seqObsId3)(s))
    } ).unsafeRunSync
  }

}
