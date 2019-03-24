// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import cats.effect.{ IO, ContextShift }
import doobie._, doobie.implicits._
import java.time.{Duration, Instant}


trait TimedSample {
  type Result

  private implicit val contextShift: ContextShift[IO] =
    IO.contextShift(scala.concurrent.ExecutionContext.global)

  val xa: Transactor[IO] =
    DatabaseConfiguration.forTesting.transactor[IO]

  def runl(args: List[String]): ConnectionIO[Result]

  def format(r: Result): String

  def main(args: Array[String]): Unit = {
    val p     = runl(args.toList)
    val start = Instant.now()
    val a     = p.transact(xa).unsafeRunSync()
    val end   = Instant.now()

    println(format(a))
    println(Duration.ofMillis(end.toEpochMilli - start.toEpochMilli))
  }
}
