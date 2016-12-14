package edu.gemini.seqexec.web.client.model

import diode.ActionResult.ModelUpdate
import diode.RootModelRW
import edu.gemini.seqexec.model.Model.SequenceView
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz._
import Scalaz._

class SeqexecCircuitSequenceDisplayHandlerSpec extends FlatSpec with Matchers with PropertyChecks with ArbitrariesWebClient {
  import edu.gemini.seqexec.model.SharedModelArbitraries._

  // Reduce the space search as List[Sequence] can be pretty large
  /*implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSize = 10, sizeRange = 20)*/

  "SequenceDisplayHandler" should /*"ignore setting a sequence for an unknown sequence" in {
    forAll { (sequence: SequenceView) =>
      val seq = sequence//.copy(id = "")
      val handler = new SequenceDisplayHandler(new RootModelRW(SequencesOnDisplay.empty))
      val result = handler.handle(SelectToDisplay(seq))
      result should matchPattern {
        case ModelUpdate(SequencesOnDisplay(t)) if t.findNext(_.sequence === SeqexecCircuit.sequenceRef(seq.id)).isEmpty =>
      }
    }
  }
  it should */"set the current sequence when present" in {
    forAll { (sod: SequencesOnDisplay) =>
      val sequenceTab = sod.instrumentSequences.rights.headOption.orElse(sod.instrumentSequences.lefts.headOption).getOrElse(sod.instrumentSequences.focus)
      val sequence = sequenceTab.sequence()
      println("SEQ " + sod)
      val handler = new SequenceDisplayHandler(new RootModelRW(sod))
      //println("FOC " + handler.modelRW.apply().instrumentSequences.focus)
      sequence.fold(fail()) { s =>
        println("FOC " + handler.modelRW.apply().instrumentSequences.toStream.toList.map(_.sequence().forall(_.id == s.id)))
        //println("SEQ " + s)
        val result = handler.handle(SelectToDisplay(s))
        println(result)
        result should matchPattern {
          case ModelUpdate(SequencesOnDisplay(t)) if t.focus === sequenceTab =>
        }
      }
    }
  }

}
