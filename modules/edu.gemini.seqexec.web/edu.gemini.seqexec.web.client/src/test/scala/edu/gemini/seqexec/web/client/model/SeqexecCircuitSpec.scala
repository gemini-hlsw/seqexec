package edu.gemini.seqexec.web.client.model

import diode.ActionResult.ModelUpdate
import diode.RootModelRW
import diode.data.{Empty, Ready}
import edu.gemini.seqexec.web.common.{Arbitraries, SeqexecQueue, SequenceInQueue}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class SeqexecCircuitSpec extends FlatSpec with Matchers with PropertyChecks with Arbitraries {
  def build = new QueueHandler(new RootModelRW(Empty))

  "SeqexecCircuit" should "support queue updates" in {
    forAll { (sequences: List[SequenceInQueue]) =>
      val result = build.handle(UpdateQueue(Ready(SeqexecQueue(sequences))))
      result should matchPattern {
        case ModelUpdate(Ready(SeqexecQueue(q))) if q == sequences =>
      }
    }
  }
}
