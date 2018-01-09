// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import cats.effect.IO
import cats.tests.CatsSuite
import fs2.Stream

@SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Equals"))
final class HorizonsClientSpec extends CatsSuite {

  def exclusionTest(s: Stream[IO, _]): Boolean = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val instrumented = { var x = 0; s.flatMap(_ => Stream.eval(IO { x += 1; Thread.sleep(10); x })) }
    val cs = Stream(instrumented).repeat.covary[IO].take(10)
    cs.joinUnbounded.compile.toVector.unsafeRunSync == (1 to 10).toList
  }

  test("Arbitrary IO should be parallel.") {
    assert(!exclusionTest(Stream.eval(IO.unit)))
  }

  test("Access to client should be serial.") {
    assert(exclusionTest(HorizonsClient.client))
  }

}
