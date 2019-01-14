// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client

import cats.Applicative
import cats.Functor
import cats.implicits._
import cats.effect.Sync
import cats.effect.Resource
import cats.effect.Effect
import cats.effect.ConcurrentEffect
import cats.effect.implicits._
import edu.gemini.aspen.giapi.status.StatusHandler
import edu.gemini.aspen.giapi.status.StatusItem
import edu.gemini.aspen.giapi.statusservice.StatusHandlerAggregate

/////////////////////////////////////////////////////////////////
// Links status streaming with the giapi db
/////////////////////////////////////////////////////////////////
trait GiapiStatusDb[F[_]] {
  def value(i: String): F[Option[StatusValue]]
}

object GiapiStatusDb {
  def newStatusDb[F[_]: ConcurrentEffect, G[_]: Functor](
    agg:   StatusHandlerAggregate,
    items: List[String]): F[GiapiStatusDb[F]] = {

    def streamItems(
      db: GiapiDb[F]
    ): F[Resource[F, StatusHandler]] =
      Sync[F].delay {

        def statusHandler = new StatusHandler {

          override def update[B](item: StatusItem[B]): Unit =
            // Check the item name and attempt convert it to A
            if (items.contains(item.getName)) {
              val r = Option(item.getValue) match {
                case Some(a: Int) =>
                  db.update[Int](item.getName, a)
                case Some(a: String) =>
                  db.update[String](item.getName, a)
                case Some(a: Float) =>
                  db.update[Float](item.getName, a)
                case Some(a: Double) =>
                  db.update[Double](item.getName, a)
                case _ =>
                  Applicative[F].unit
              }
              r.toIO.unsafeRunAsync(_ => ())
            }

          override def getName: String = "StatusHandler"
        }

        // A trivial resource that binds and unbinds a status handler.
        Resource.make(
          Effect[F].delay {
            val sh = statusHandler
            agg.bindStatusHandler(sh)
            sh
          }
        )(sh => Effect[F].delay(agg.unbindStatusHandler(sh)))
      }

    for {
      db <- GiapiDb.newDb
      _  <- streamItems(db)
    } yield
      new GiapiStatusDb[F] {
        def value(i: String): F[Option[StatusValue]] =
          db.value(i)
      }
  }

}
