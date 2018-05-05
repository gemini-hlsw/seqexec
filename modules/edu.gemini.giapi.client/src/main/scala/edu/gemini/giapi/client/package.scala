// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.giapi

import cats.implicits._
import cats.effect._
import edu.gemini.aspen.giapi.util.jms.status.StatusGetter
import edu.gemini.jms.activemq.provider.ActiveMQJmsProvider
import fs2.async

package object client {

  /**
    * Allowed types according to GIAPI
    */
  implicit val strItemGetter: ItemGetter[String]    = new ItemGetter[String] {}
  implicit val doubleItemGetter: ItemGetter[Double] = new ItemGetter[Double] {}
  implicit val intItemGetter: ItemGetter[Int]       = new ItemGetter[Int]    {}
  implicit val floatItemGetter: ItemGetter[Float]   = new ItemGetter[Float]  {}
}

package client {

  /**
    * Typeclass to present as evidence when calling `Giapi.get`
    */
  sealed trait ItemGetter[A]

  /**
    * Represents a connection to a GIAPi based instrument
    *
    * @tparam F Effect type
    */
  trait GiapiConnection[F[_]] {
    def connect: F[Giapi[F]]
  }

  /**
    * Algebra to interact with a GIAPI instrument
    *
    * @tparam F Effect Type
    */
  trait Giapi[F[_]] {

    /**
      * Returns a value for the status item. If not found or there is an error, e.g. on types the exception is returned
      */
    def get[A: ItemGetter](statusItem: String): F[Option[A]]

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
      * Interpreter on F
      *
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

        private def giapi(c: ActiveMQJmsProvider, sg: StatusGetter) =
          new Giapi[F] {
            override def get[A: ItemGetter](statusItem: String): F[Option[A]] =
              Sync[F].delay {
                val item = sg.getStatusItem[A](statusItem)
                Either.catchNonFatal(item.getValue).toOption
              }

            override def close: F[Unit] =
              for {
                _ <- Sync[F].delay(sg.stopJms())
                _ <- Sync[F].delay(c.stopConnection())
              } yield ()
          }

        private def build(ref: async.Ref[F, ActiveMQJmsProvider]): F[Giapi[F]] =
          for {
            c  <- ref.get
            sg <- statusGetter(c)
          } yield giapi(c, sg)

        def connect: F[Giapi[F]] =
          for {
            c   <- Sync[F].delay(new ActiveMQJmsProvider(url)) // Build the connection
            ref <- async.refOf(c)                              // store a reference
            _   <- Sync[F].delay(c.startConnection())          // Start the connection
            c   <- build(ref)                                  // Build the interpreter
          } yield c
      }
  }

}
