package gem

import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

trait Arbitraries {

  implicit val arbLocation: Arbitrary[Location] =
    Arbitrary { arbitrary[List[Int]].map(Location.fromList) }

}
