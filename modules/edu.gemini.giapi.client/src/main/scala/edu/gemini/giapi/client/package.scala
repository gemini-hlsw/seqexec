// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.giapi.client

import cats.{Id, Monad}
import cats.implicits._

trait Giapi[F[_]] {
  def init: F[Unit]
  def get(statusItem: String): F[Option[Int]]
}

object Giapi {
  val giapiWithId: Giapi[Id] = new Giapi[Id] {
    def init: Id[Unit] = ()
    def get(statusItem: String): Id[Option[Int]] = {println("Id");None}
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
}
