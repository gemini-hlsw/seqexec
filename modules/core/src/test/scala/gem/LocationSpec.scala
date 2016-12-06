package gem

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random
import scalaz._, Scalaz._

class LocationSpec extends FlatSpec with Matchers with PropertyChecks with Arbitraries {
  "construction" should "trim trailing min values" in {
    forAll { (l: Location) =>
      val t = l.toIList
      t shouldEqual t.dropRightWhile(_ == Int.MinValue)
    }
  }

  "EQ" should "agree with ==" in {
    forAll { (l0: Location, l1: Location) =>
      Order[Location].equal(l0, l1) shouldEqual (l0 == l1)
    }
  }
}

object LocationSpec {
  private val ws = Array(' ', '\t', '\n', '\f', '\r')
  private val r  = new Random
  private def randomWhitespace: String =
    new String((0 to r.nextInt(10)).toArray.map(_ => ws(r.nextInt(ws.size))))
}
