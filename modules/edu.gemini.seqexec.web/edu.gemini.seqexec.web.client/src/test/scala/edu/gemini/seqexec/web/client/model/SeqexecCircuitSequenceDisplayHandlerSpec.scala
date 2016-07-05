package edu.gemini.seqexec.web.client.model

import diode.ActionResult.ModelUpdate
import diode.RootModelRW
import edu.gemini.seqexec.web.common.Sequence
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz._
import Scalaz._

class SeqexecCircuitSequenceDisplayHandlerSpec extends FlatSpec with Matchers with PropertyChecks with ArbitrariesWebClient {

  // Reduce the space search as List[Sequence] can be pretty large
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSize = 10, sizeRange = 20)

  "SeqexecCircuit SequenceDisplayHandler" should "ignore setting a sequence for an uknown sequence" in {
    forAll { (sequence: Sequence) =>
        val handler = new SequenceDisplayHandler(new RootModelRW(SequencesOnDisplay.empty))
        val result = handler.handle(SelectToDisplay(sequence))
        result should matchPattern {
          case ModelUpdate(SequencesOnDisplay(t)) if t.findNext(_.sequence == SeqexecCircuit.sequenceRef(sequence.id)).isEmpty =>
        }
      }
    }

}
