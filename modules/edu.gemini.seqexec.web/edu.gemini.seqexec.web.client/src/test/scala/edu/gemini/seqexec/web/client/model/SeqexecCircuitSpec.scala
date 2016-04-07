package edu.gemini.seqexec.web.client.model

import diode.ActionResult.{ModelUpdate, ModelUpdateEffect}
import diode.RootModelRW
import diode.data._
import edu.gemini.seqexec.web.common.{Arbitraries, SeqexecQueue, Sequence}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class SeqexecCircuitSpec extends FlatSpec with Matchers with PropertyChecks with Arbitraries {

  def build = new QueueHandler(new RootModelRW(Empty))

  // Reduce the space search as List[Sequence] can be pretty large
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSize = 10, sizeRange = 20)

  "SeqexecCircuit" should
    "support queue updates" in {
    forAll { (sequences: List[Sequence]) =>
      val result = build.handle(UpdatedQueue(Ready(SeqexecQueue(sequences))))
      result should matchPattern {
        case ModelUpdate(Ready(SeqexecQueue(q))) if q == sequences =>
      }
    }
  }
  it should "support pending state" in {
    val result = build.handle(UpdatedQueue(Empty))
    result should matchPattern {
      case ModelUpdateEffect(newValue: Pot[_], effects) if newValue.isPending && effects.size == 2 =>
    }
  }
  it should "support error case" in {
    val result = build.handle(UpdatedQueue(Failed(new RuntimeException("error"))))
    result should matchPattern {
      case ModelUpdate(newValue: Pot[_]) if newValue.isFailed =>
    }
  }

}
