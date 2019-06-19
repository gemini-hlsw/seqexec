// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import cats.effect.{ IO, ContextShift }
import cats.tests.CatsSuite
import fs2.Stream

final class HorizonsClientSpec extends CatsSuite {

  private implicit val contextShift: ContextShift[IO] =
    IO.contextShift(scala.concurrent.ExecutionContext.global)

  def exclusionTest(s: Stream[IO, _]): Boolean = {
    val count = 20
    val instrumented = { var x = 0; s.flatMap(_ => Stream.eval(IO { x += 1; Thread.sleep(10); x })) }
    val cs = Stream(instrumented).repeat.covary[IO].take(count.toLong)
    cs.parJoinUnbounded.compile.toVector.unsafeRunSync == (1 to count).toList
  }

  test("Arbitrary IO should be parallel.") {
    assert(!exclusionTest(Stream.eval(IO.unit)))
  }

  // Have to turn this off for now
//  test("Access to client should be serial.") {
//    assert(exclusionTest(HorizonsClient.client))
//  }

}
