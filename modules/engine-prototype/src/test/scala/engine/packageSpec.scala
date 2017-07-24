package engine

import org.scalatest._

class engineSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {
    engine.run shouldEqual "hello"
  }
}
