package edu.gemini.seqexec.web.client.model

import diode.ActionResult.ModelUpdate
import diode.RootModelRW
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz._
import Scalaz._

class SeqexecCircuitSequenceDisplayHandlerSpec extends FlatSpec with Matchers with PropertyChecks with ArbitrariesWebClient {

  // Reduce the space search as List[Sequence] can be pretty large
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSize = 10, sizeRange = 20)

  "SeqexecCircuit SequenceDisplayHandler" should "support focusing on a sequence" in {
    forAll { (sequenceTabs: List[SequenceTab]) =>
      whenever(sequenceTabs.nonEmpty) {
        val currentTabs = NonEmptyList(sequenceTabs.head, sequenceTabs.tail: _*)
        val tabsZipper = currentTabs.toZipper
        val handler = new SequenceDisplayHandler(new RootModelRW(SequencesOnDisplay(tabsZipper)))
        tabsZipper.focus should equal(sequenceTabs.head)
        val last = currentTabs.last
        // We can only focus if there is a sequence
        last match {
          case tab @ SequenceTab(_, Some(s)) =>
            val result = handler.handle(SelectToDisplay(s))
            result should matchPattern {
              case ModelUpdate(SequencesOnDisplay(t)) if t.focus == tab =>
            }
          case _                             => // In case there is no sequence for a tab, you can't focus it
        }
      }
    }
  }

}
