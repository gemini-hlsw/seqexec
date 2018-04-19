// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.giapi.client

import cats.{Id, Monad}
import cats.implicits._
import cats.effect._
import edu.gemini.jms.activemq.provider.ActiveMQJmsProvider

trait Giapi[F[_]] {
  def init: F[Unit]
  def get(statusItem: String): F[Option[Int]]
}

object Giapi {
  val giapiWithId: Giapi[Id] = new Giapi[Id] {
    def init: Id[Unit] = ()
    def get(statusItem: String): Id[Option[Int]] = {println("Id");None}
  }

  val giapiWithIO: Giapi[IO] = new Giapi[IO] {
    val connection = new ActiveMQJmsProvider("failover:(tcp://tlc.cl.gemini.edu:61616)")
    connection.startConnection()
    def init: IO[Unit] = IO.pure(())
    def get(statusItem: String): IO[Option[Int]] = IO{println("Id");None}
  }

}

object Example extends App {
  // Read an item from GPI
  class GPIRead[F[_]: Monad](giapi: Giapi[F]) {
    def readFilter: F[Option[Int]] = {
      for {
        _ <- giapi.init
        v <- giapi.get("abc")
      } yield v
    }
  }

  println(new GPIRead(Giapi.giapiWithId).readFilter)
  println(new GPIRead(Giapi.giapiWithIO).readFilter)
}
