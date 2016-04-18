package edu.gemini.seqexec.web.client.model

import diode.ActionResult.{ModelUpdate, ModelUpdateEffect}
import diode.RootModelRW
import diode.data._
import edu.gemini.seqexec.web.common._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class SeqexecCircuitQueueHandlerSpec extends FlatSpec with Matchers with PropertyChecks with ArbitrariesWebCommon {

  def emptyQueueHandler = new QueueHandler(new RootModelRW(Empty))

  // Filter out a list of sequence for possible duplicates on the id
  def filterDistinct(initial: List[Sequence]):List[Sequence] = initial.groupBy(_.id).map(_._2.head).toList

  // Reduce the space search as List[Sequence] can be pretty large
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSize = 10, sizeRange = 20)

  "SeqexecCircuit QueueHandler" should "support queue updates" in {
    forAll { (sequences: List[Sequence]) =>
      val result = emptyQueueHandler.handle(UpdatedQueue(Ready(SeqexecQueue(sequences))))
      result should matchPattern {
        case ModelUpdate(Ready(SeqexecQueue(q))) if q == sequences =>
      }
    }
  }
  it should "support pending state" in {
    val result = emptyQueueHandler.handle(UpdatedQueue(Empty))
    result should matchPattern {
      case ModelUpdateEffect(newValue: Pot[_], effects) if newValue.isPending && effects.size == 2 =>
    }
  }
  it should "support error case" in {
    val result = emptyQueueHandler.handle(UpdatedQueue(Failed(new RuntimeException("error"))))
    result should matchPattern {
      case ModelUpdate(newValue: Pot[_]) if newValue.isFailed =>
    }
  }

  "SeqexecCircuit QueueHandler" should "support adding a sequence when empty" in {
    forAll { (sequence: Sequence) =>
      val queueHandler = new QueueHandler(new RootModelRW(Ready(SeqexecQueue(Nil))))
      // add one
      val result = queueHandler.handle(AddToQueue(sequence))
      result should matchPattern {
        case ModelUpdate(Ready(SeqexecQueue(q))) if q.size == 1 => // Add an item to the queue
      }
    }
  }
  it should "support adding a sequence" in {
    forAll { (initial: List[Sequence], sequence: Sequence) =>
      whenever(!initial.contains(sequence)) {
        val distinct = filterDistinct(initial)
        val queueHandler = new QueueHandler(new RootModelRW(Ready(SeqexecQueue(distinct))))
        val result = queueHandler.handle(AddToQueue(sequence))
        result should matchPattern {
          case ModelUpdate(Ready(SeqexecQueue(q))) if q.size == distinct.size + 1 => // added a new item to the list
        }
      }
    }
  }
  it should "ignore adding an existing sequence" in {
    forAll { (initial: List[Sequence]) =>
      whenever(initial.nonEmpty) {
        // Remove duplicates by id
        val distinct = filterDistinct(initial)
        val queueHandler = new QueueHandler(new RootModelRW(Ready(SeqexecQueue(distinct))))
        // Sequences are checked by id
        val sequence = Sequence(distinct.head.id, SequenceState.NotRunning, "", SequenceSteps(Nil), None)
        val result = queueHandler.handle(AddToQueue(sequence))
        result should matchPattern {
          case ModelUpdate(Ready(SeqexecQueue(q))) if q.size == distinct.size => // Ignore adding an existing item
        }
      }
    }
  }

}
