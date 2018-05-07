// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.giapi.client.gpi

import cats.effect.IO
import edu.gemini.giapi.client.Giapi
import fs2.Stream

/**
  * Client for GPI
  */
class GPIClient[F[_]](giapi: Giapi[F]) {

  // Some items, more will be added as needed
  def heartbeat: F[Int] =
    giapi.get[Int]("gpi:heartbeat")

  def fpmMask: F[String] =
    giapi.get[String]("gpi:fpmMask")

  def aoDarkLevel: F[Float] =
    giapi.get[Float]("gpi:ao:darkLevel")

  // add more items...
}

object GPIExample extends App {

  private val gpi =
    Stream.bracket(Giapi.giapiConnection[IO]("failover:(tcp://127.0.0.1:61616)").connect)(
      giapi => {
        val client = new GPIClient[IO](giapi)
        val r =
          for {
            h <- client.heartbeat
            f <- client.fpmMask
            o <- client.aoDarkLevel
          } yield (h, f, o)
        Stream.eval(r.map(println)) // scalastyle:ignore
      },
      _.close
    )

  gpi.compile.drain.unsafeRunSync()
}
