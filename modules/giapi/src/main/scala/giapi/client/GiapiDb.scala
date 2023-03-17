// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client

import cats.Applicative
import cats.effect.Concurrent
import cats.syntax.all._
import fs2.Stream
import fs2.concurrent.SignallingRef

sealed trait StatusValue extends Product with Serializable

object StatusValue {
  final case class IntValue(value: Int)       extends StatusValue
  final case class StringValue(value: String) extends StatusValue
  final case class FloatValue(value: Float)   extends StatusValue
  final case class DoubleValue(value: Double) extends StatusValue

  def intValue(s: StatusValue): Option[Int]       = s match {
    case IntValue(v) => ItemGetter[Int].value(v)
    case _           => none
  }
  def stringValue(s: StatusValue): Option[String] = s match {
    case StringValue(v) => ItemGetter[String].value(v)
    case _              => none
  }
  def floatValue(s: StatusValue): Option[Float]   = s match {
    case FloatValue(v) => ItemGetter[Float].value(v)
    case _             => none
  }
  def doubleValue(s: StatusValue): Option[Double] = s match {
    case DoubleValue(v) => ItemGetter[Double].value(v)
    case _              => none
  }
}

/////////////////////////////////////////////////////////////////
// The GiapiDb stores values streamed from the GMP
/////////////////////////////////////////////////////////////////
trait GiapiDb[F[_]] {

  def value(i: String): F[Option[StatusValue]]

  def update[A: ItemGetter](i: String, s: A): F[Unit]

  def discrete: Stream[F, Map[String, StatusValue]]
}

object GiapiDb {
  def newDb[F[_]: Concurrent]: F[GiapiDb[F]] =
    SignallingRef[F, Map[String, StatusValue]](Map.empty).map { ref =>
      new GiapiDb[F] {
        def value(i: String): F[Option[StatusValue]] =
          ref.get.map(_.get(i))

        def update[A: ItemGetter](i: String, s: A): F[Unit] =
          ItemGetter[A].value(s) match {
            case Some(a: Int)    =>
              ref.update(_ + (i -> StatusValue.IntValue(a)))
            case Some(a: String) =>
              ref.update(_ + (i -> StatusValue.StringValue(a)))
            case Some(a: Float)  =>
              ref.update(_ + (i -> StatusValue.FloatValue(a)))
            case Some(a: Double) =>
              ref.update(_ + (i -> StatusValue.DoubleValue(a)))
            case _               =>
              Applicative[F].unit
          }

        override def discrete: Stream[F, Map[String, StatusValue]] = ref.discrete
      }
    }
}
