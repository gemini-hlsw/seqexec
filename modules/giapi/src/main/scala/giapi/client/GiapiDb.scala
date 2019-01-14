// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import cats.Applicative
import monocle.Lens
import monocle.function.At.at
import monocle.function.At.atMap

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
  def newDb[F[_]: Sync](): F[GiapiDb[F]] =
    Ref.of[F, Map[String, StatusValue]](Map.empty).map { ref =>
      def dbat(i: String): Lens[Map[String, StatusValue], Option[StatusValue]] =
        at(i)

      new GiapiDb[F] {
        def value(i: String): F[Option[StatusValue]] =
          ref.get.map(_.get(i))

        def update[A: ItemGetter](i: String, s: A): F[Unit] =
          ItemGetter[A].value(s) match {
            case Some(a: Int) =>
              ref.update(dbat(i).set(StatusValue.IntValue(a).some))
            case Some(a: String) =>
              ref.update(dbat(i).set(StatusValue.StringValue(a).some))
            case Some(a: Float) =>
              ref.update(dbat(i).set(StatusValue.FloatValue(a).some))
            case Some(a: Double) =>
              ref.update(dbat(i).set(StatusValue.DoubleValue(a).some))
            case _ =>
              Applicative[F].unit
          }
      }
    }
}
