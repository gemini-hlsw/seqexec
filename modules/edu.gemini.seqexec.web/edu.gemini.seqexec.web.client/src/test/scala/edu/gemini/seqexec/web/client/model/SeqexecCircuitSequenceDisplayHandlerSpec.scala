package edu.gemini.seqexec.web.client.model

import diode.ActionResult.{ModelUpdate, ModelUpdateEffect}
import diode.RootModelRW
import diode.data._
import edu.gemini.seqexec.web.common.{ArbitrariesWebCommon, Sequence}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class SeqexecCircuitSequenceDisplayHandlerSpec extends FlatSpec with Matchers with PropertyChecks with ArbitrariesWebClient {

  // Reduce the space search as List[Sequence] can be pretty large
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSize = 10, sizeRange = 20)

  "SeqexecCircuit SequenceDisplayHandler" should "support focusing on a sequence" in {
    forAll { (sequenceTabs: List[SequenceTab]) =>
      whenever(sequenceTabs.nonEmpty) {
        val handler = new SequenceDisplayHandler(new RootModelRW(SequencesOnDisplay(sequenceTabs, Nil, sequenceTabs.head)))
        val last = sequenceTabs.last
        // We can only focus if there is a sequence
        last match {
          case tab @ SequenceTab(_, Some(s)) =>
            val result = handler.handle(SelectToDisplay(s))
            result should matchPattern {
              case ModelUpdate(SequencesOnDisplay(t, Nil, f)) if t == sequenceTabs && f == tab =>
            }
          case _                             =>
        }
      }
    }
  }

}
