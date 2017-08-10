// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2.pio

import cats.Functor
import cats.implicits._
import java.time.{ Duration, Instant }
import scala.util.Try

final case class PioParse[A](run: String => Option[A]) {
  def apply(s: String): Option[A] = run(s)
}

object PioParse {

  implicit val FunctorPioParse: Functor[PioParse] =
    new Functor[PioParse] {
      def map[A, B](pa: PioParse[A])(f: A => B): PioParse[B] =
        PioParse(pa.run andThen (_.map(f)))
    }

  def enum[A](dictionary: (String, A)*): PioParse[A] =
    PioParse(dictionary.toMap.lift)

  // ********  Primitives

  val bigDecimal: PioParse[BigDecimal] =
    PioParse(s => Try(BigDecimal(s)).toOption)

  val boolean: PioParse[Boolean] =
    PioParse(s => Try(s.toBoolean).toOption)

  val double: PioParse[Double] =
    PioParse(s => Try(s.toDouble).toOption)

  val short: PioParse[Short] =
    PioParse(s => Try(s.toShort).toOption)

  val int: PioParse[Int] =
    PioParse(s => Try(s.toInt).toOption)

  val positiveInt: PioParse[Int] =
    PioParse(s => Try(s.toInt).toOption.filter(_ > 0))

  val positiveShort: PioParse[Short] =
    PioParse(s => Try(s.toShort).toOption.filter(_ > 0))

  val long: PioParse[Long] =
    PioParse(s => Try(s.toLong).toOption)

  val string: PioParse[String] =
    PioParse(_.pure[Option])


  // ********  Java

  val instant: PioParse[Instant] =
    long.map(Instant.ofEpochMilli)

  val seconds: PioParse[Duration] =
    double.map { d => Duration.ofMillis((d * 1000).round) }
}
