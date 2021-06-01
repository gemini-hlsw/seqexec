// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client

import scala.jdk.CollectionConverters._

import cats.Applicative
import cats.ApplicativeError
import cats.effect.ConcurrentEffect
import cats.effect.Effect
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.implicits._
import cats.syntax.all._
import edu.gemini.aspen.giapi.status.StatusHandler
import edu.gemini.aspen.giapi.status.StatusItem
import edu.gemini.aspen.giapi.statusservice.StatusHandlerAggregate
import edu.gemini.aspen.giapi.util.jms.status.StatusGetter
import edu.gemini.jms.activemq.provider.ActiveMQJmsProvider
import fs2.Stream
import fs2.concurrent.Queue

/////////////////////////////////////////////////////////////////
// Links status streaming with the giapi db
/////////////////////////////////////////////////////////////////
trait GiapiStatusDb[F[_]] {

  // Tries to read a value from the db and returns non if not found
  def optional(i: String): F[Option[StatusValue]]

  // Tries to read a value from the db and throws an exception in not found
  def value(i: String): F[StatusValue]

  def discrete: Stream[F, Map[String, StatusValue]]

  private[giapi] def close: F[Unit]
}

object GiapiStatusDb {
  private def dbUpdate[F[_]: Applicative](
    db:   GiapiDb[F],
    name: String,
    a:    Any
  ): F[Unit] =
    a match {
      case a: Int    =>
        db.update(name, a)
      case a: String =>
        db.update(name, a)
      case a: Float  =>
        db.update(name, a)
      case a: Double =>
        db.update(name, a)
      case _         =>
        Applicative[F].unit
    }

  private def streamItemsToDb[F[_]: ConcurrentEffect](
    agg:   StatusHandlerAggregate,
    db:    GiapiDb[F],
    items: List[String]
  ): F[Unit] = {
    def statusHandler(q: Queue[F, (String, Any)]) = new StatusHandler {

      override def update[B](item: StatusItem[B]): Unit =
        // Check the item name and enqueue it
        if (items.contains(item.getName)) {
          Option(item.getValue).foreach { a =>
            q.enqueue1((item.getName, a)).toIO.unsafeRunAsync(_ => ())
          }
        }

      override def getName: String = "Giapi staus db"
    }

    // A trivial resource that binds and unbinds a status handler.
    def bind(q: Queue[F, (String, Any)]): Resource[F, StatusHandler] =
      Resource.make(
        Effect[F].delay {
          val sh = statusHandler(q)
          agg.bindStatusHandler(sh)
          sh
        }
      )(sh => Effect[F].delay(agg.unbindStatusHandler(sh)))

    // Create a queue and put updates to forward them to the db
    val s = for {
      q <- Stream.eval(Queue.unbounded[F, (String, Any)])
      _ <- Stream.resource(bind(q))
      _ <- q.dequeue.evalMap { case (n, v) => dbUpdate(db, n, v) }
    } yield ()
    s.compile.drain
  }

  private def initSG[F[_]: Applicative](
    db:    GiapiDb[F],
    sg:    StatusGetter,
    items: List[String]
  ): F[List[Unit]] =
    sg.getAllStatusItems.asScala.toList
      .collect {
        case s: StatusItem[_] if items.contains(s.getName) => s
      }
      .traverse { s =>
        dbUpdate(db, s.getName, s.getValue)
      }

  private def initDb[F[_]: Sync](
    c:     ActiveMQJmsProvider,
    db:    GiapiDb[F],
    items: List[String]
  ): F[List[Unit]] =
    Resource
      .make(Giapi.statusGetter[F](c))(g => Sync[F].delay(g.stopJms()))
      .use(initSG(db, _, items))

  /**
   * Creates a new status db in simulation
   */
  def simulatedDb[F[_]: ApplicativeError[*[_], Throwable]]: GiapiStatusDb[F] =
    new GiapiStatusDb[F] {
      def optional(i: String): F[Option[StatusValue]] =
        none.pure[F]

      def value(i: String): F[StatusValue] =
        ApplicativeError[F, Throwable]
          .raiseError(
            new GiapiException("No values available in a simulated db")
          )

      def discrete: Stream[F, Map[String, StatusValue]] =
        Stream.empty

      def close: F[Unit] = Applicative[F].unit
    }

  /**
   * Creates a new status db that listens for status items as they are produced
   *
   * @param url Url of the giapi server
   * @param items List of items to monitor
   */
  def newStatusDb[F[_]: ConcurrentEffect](
    url:   String,
    items: List[String]
  ): F[GiapiStatusDb[F]] =
    for {
      c  <- Sync[F].delay(new ActiveMQJmsProvider(url)) // Build the connection
      ss <- Giapi.statusStreamer[F](c) // giapi artifacts
      db <- GiapiDb.newDb
      _  <- initDb[F](c, db, items) // Get the initial values
      f  <- streamItemsToDb[F](ss.aggregate, db, items).start // run in the background
    } yield new GiapiStatusDb[F] {
      def optional(i: String): F[Option[StatusValue]] =
        db.value(i)

      def value(i: String): F[StatusValue] =
        optional(i)
          .ensure(new GiapiException(s"Giapi channel $i not found"))(
            _.isDefined
          )
          .map {
            _.orNull
          } // orNull lets us typecheck but it will never be used due to the `ensure` call above

      def discrete: Stream[F, Map[String, StatusValue]] =
        db.discrete

      def close: F[Unit] =
        for {
          _ <- Sync[F].delay(ss.ss.stopJms()) // Close the listener
          _ <- Sync[F].delay(c.stopConnection()) // Disconnect from amq
          _ <- Sync[F].delay(f.cancel) // Stop the fiber
        } yield ()
    }

}
