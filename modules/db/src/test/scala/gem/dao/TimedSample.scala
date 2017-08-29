// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import cats.effect.IO
import doobie._, doobie.implicits._
import java.time.{Duration, Instant}


trait TimedSample {
  type Result

  val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:gem",
    "postgres",
    ""
  )

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
