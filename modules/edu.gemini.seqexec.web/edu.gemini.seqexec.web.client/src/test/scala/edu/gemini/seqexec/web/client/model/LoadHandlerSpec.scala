package edu.gemini.seqexec.web.client.model

import diode.ActionResult.ModelUpdateEffect
import diode.RootModelRW
import diode.data._
import edu.gemini.seqexec.web.common.ArbitrariesWebCommon
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class LoadHandlerSpec extends FlatSpec with Matchers with PropertyChecks with ArbitrariesWebCommon {

  // Reduce the space search as List[Sequence] can be pretty large
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSize = 10, sizeRange = 20)

  "SeqexecCircuit LoadHandler" should "support load" in {
    def handler = new LoadHandler(new RootModelRW(Empty))
    val result = handler.handle(LoadSequence("", Empty))
    result should matchPattern {
      case ModelUpdateEffect(newValue: Pot[_], effects) if newValue.isPending && effects.size == 1 =>
    }
  }

}
