package edu.gemini.seqexec.web.client.model

import diode.ActionResult.ModelUpdate
import diode.RootModelRW
import edu.gemini.seqexec.model.Model.{SeqexecSite, SequenceView}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz._
import Scalaz._

class SequenceDisplayHandlerSpec extends FlatSpec with Matchers with PropertyChecks with ArbitrariesWebClient {
  import edu.gemini.seqexec.model.SharedModelArbitraries._

  "SequenceDisplayHandler" should "ignore setting a sequence for an unknown sequence" in {
    forAll { (sequence: SequenceView) =>
      val handler = new SequenceDisplayHandler(new RootModelRW((SequencesOnDisplay.empty, SeqexecUIModel.noSequencesLoaded, SeqexecSite.SeqexecGS)))
      val result = handler.handle(SelectToDisplay(sequence))
      result should matchPattern {
        case ModelUpdate((SequencesOnDisplay(t), _, _)) if t.findNext(_.sequence === SeqexecCircuit.sequenceRef(sequence.id)).isEmpty =>
      }
    }
  }

}
