// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.giapi.client

import cats._
import cats.implicits._
import cats.effect._
import edu.gemini.aspen.giapi.util.jms.status.StatusGetter
import edu.gemini.jms.activemq.provider.ActiveMQJmsProvider
import fs2.{Stream, async}
import fs2.async.Ref

/**
  * Represents a connection to a GIAPi based instrument
  * @tparam F Effect type
  */
trait GiapiConnection[F[_]] {
  def connect: F[Giapi[F]]
}

/**
  * Algebra to interact with a GIAPI instrument
  * @tparam F Effect Type
  */
trait Giapi[F[_]] {

  /**
    * Returns a value for the status item. If not found a `None` is returned
    */
  def get(statusItem: String): F[Option[Int]]

  /**
    * Close the connection
    */
  def close: F[Unit]
}

/**
  * Interpreters
  */
object Giapi {

  /**
    * Interpreter on Id
    */
  val giapiConnectionWithId: GiapiConnection[Id] = new GiapiConnection[Id] {

    def connect: Id[Giapi[Id]] = new Giapi[Id] {

      def get(statusItem: String): Id[Option[Int]] =
        None

      override def close: Id[Unit] = ()
    }
  }

  /**
    * Interpreter on F
    * @param url Url to connect to
    * @tparam F Effect type
    */
  def giapiConnection[F[_]: Sync](url: String): GiapiConnection[F] =
    new GiapiConnection[F] {
      private def statusGetter(c: ActiveMQJmsProvider): F[StatusGetter] = Sync[F].delay {
        val sg = new StatusGetter("client")
        sg.startJms(c)
        sg
      }

      private def build(ref: Ref[F, ActiveMQJmsProvider]): F[Giapi[F]] =
        for {
          c  <- ref.get
          sc <- statusGetter(c)
        } yield
          // Build a reference
          new Giapi[F] {
            override def get(statusItem: String): F[Option[Int]] = Sync[F].delay {
              val item = sc.getStatusItem(statusItem)
              val r = item.getValue match {
                case i: Int => i
                case _      => -1
              }
              Some(r)
            }

            override def close: F[Unit] =
              for {
                _ <- Sync[F].delay(sc.stopJms())
                _ <- Sync[F].delay(c.stopConnection())
              } yield ()
          }

      def connect: F[Giapi[F]] =
        for {
          c   <- Sync[F].delay(new ActiveMQJmsProvider(url)) // Build the connection
          ref <- async.refOf(c)                              // store a reference
          _   <- Sync[F].delay(c.startConnection())          // Start the connection
          c   <- build(ref)                                  // Build the interpreter
        } yield c
    }
}

object Example extends App {

  // Read an item from GPI
  class GPIRead[F[_]: Monad](giapi: Giapi[F]) {

    def readFilter: F[Option[Int]] =
      for {
        v <- giapi.get("gmp:gmp:heartbeat")
      } yield v
  }

  private val gpi =
    Stream.bracket(Giapi.giapiConnection[IO]("failover:(tcp://172.16.140.131:61616)").connect)(
      giapi => {
        val r =
          for {
            v <- new GPIRead(giapi).readFilter
          } yield println(v) // scalastyle:off
        Stream.eval(r)
      },
      _.close)

  gpi.compile.drain.unsafeRunSync()
}
