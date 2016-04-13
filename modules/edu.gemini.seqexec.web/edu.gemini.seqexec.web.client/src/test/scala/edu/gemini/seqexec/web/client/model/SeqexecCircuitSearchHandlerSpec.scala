package edu.gemini.seqexec.web.client.model

import diode.ActionResult.{ModelUpdate, ModelUpdateEffect}
import diode.RootModelRW
import diode.data._
import edu.gemini.seqexec.web.common.{ArbitrariesWebCommon, Sequence}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class SeqexecCircuitSearchHandlerSpec extends FlatSpec with Matchers with PropertyChecks with ArbitrariesWebCommon {

  // Reduce the space search as List[Sequence] can be pretty large
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSize = 10, sizeRange = 20)

  "SeqexecCircuit SearchHandler" should "support search" in {
    def handler = new SearchHandler(new RootModelRW(Empty))
    val result = handler.handle(SearchSequence("", Empty))
    result should matchPattern {
      case ModelUpdateEffect(newValue: Pot[_], effects) if newValue.isPending && effects.size == 2 =>
    }
  }
  it should "should ignore removing if empty" in {
    forAll { (sequence: Sequence) =>
      def handler = new SearchHandler(new RootModelRW(Ready(Nil)))
      val result = handler.handle(RemoveFromSearch(sequence))
      result should matchPattern {
        case ModelUpdate(Ready(s: List[_])) if s.isEmpty =>
      }
    }
  }
  it should "should support removing from results if contained" in {
    forAll { (sequences: List[Sequence]) =>
      whenever(sequences.nonEmpty) {
        def handler = new SearchHandler(new RootModelRW(Ready(sequences)))
        val removed = sequences.head
        val result = handler.handle(RemoveFromSearch(removed))
        result should matchPattern {
          case ModelUpdate(Ready(s: List[_])) if !s.contains(removed) =>
        }
      }
    }
  }

}
