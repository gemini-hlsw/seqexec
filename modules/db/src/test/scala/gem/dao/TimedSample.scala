// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import doobie.imports._

import java.time.{Duration, Instant}

import scalaz.effect.IO

trait TimedSample {
  type Result

  val xa = DriverManagerTransactor[IO](
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
    val a     = p.transact(xa).unsafePerformIO()
    val end   = Instant.now()

    println(format(a))
    println(Duration.ofMillis(end.toEpochMilli - start.toEpochMilli))
  }
}
