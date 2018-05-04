// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.giapi.client

import cats._
import cats.implicits._
import cats.effect._
import edu.gemini.aspen.giapi.util.jms.status.StatusGetter
import edu.gemini.jms.activemq.provider.ActiveMQJmsProvider
import fs2.async

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
  def get[A](statusItem: String): F[Option[A]]

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

      def get[A](statusItem: String): Id[Option[A]] =
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

      private def build(ref: async.Ref[F, ActiveMQJmsProvider]): F[Giapi[F]] =
        for {
          c  <- ref.get
          sc <- statusGetter(c)
        } yield
        // Build a reference
        new Giapi[F] {
          override def get[A](statusItem: String): F[Option[A]] = Sync[F].delay {
            val item = sc.getStatusItem[A](statusItem)
            Option(item.getValue)
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

