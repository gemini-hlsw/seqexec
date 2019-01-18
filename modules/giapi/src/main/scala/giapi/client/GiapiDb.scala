// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import cats.Applicative

sealed trait StatusValue extends Product with Serializable

object StatusValue {
  final case class IntValue(value:    Int) extends StatusValue
  final case class StringValue(value: String) extends StatusValue
  final case class FloatValue(value:  Float) extends StatusValue
  final case class DoubleValue(value: Double) extends StatusValue
}

/////////////////////////////////////////////////////////////////
// The GiapiDb stores values streamed from the GMP
/////////////////////////////////////////////////////////////////
trait GiapiDb[F[_]] {

  def value(i: String): F[Option[StatusValue]]

  def update[A: ItemGetter](i: String, s: A): F[Unit]
}

object GiapiDb {
  def newDb[F[_]: Sync]: F[GiapiDb[F]] =
    Ref.of[F, Map[String, StatusValue]](Map.empty).map { ref =>
      new GiapiDb[F] {
        def value(i: String): F[Option[StatusValue]] =
          ref.get.map(_.get(i))

        def update[A: ItemGetter](i: String, s: A): F[Unit] =
          ItemGetter[A].value(s) match {
            case Some(a: Int) =>
              ref.update(_ + (i -> StatusValue.IntValue(a)))
            case Some(a: String) =>
              ref.update(_ + (i -> StatusValue.StringValue(a)))
            case Some(a: Float) =>
              ref.update(_ + (i -> StatusValue.FloatValue(a)))
            case Some(a: Double) =>
              ref.update(_ + (i -> StatusValue.DoubleValue(a)))
            case _ =>
              Applicative[F].unit
          }
      }
    }
}
