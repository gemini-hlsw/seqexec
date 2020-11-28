// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2.pio

import lucuma.core.util.Enumerated
import lucuma.core.syntax.string._

import cats.Functor
import cats.syntax.all._
import java.time.{ Duration, Instant }

final case class PioParse[A](run: String => Option[A]) {
  def apply(s: String): Option[A] = run(s)
}

object PioParse {

  implicit val FunctorPioParse: Functor[PioParse] =
    new Functor[PioParse] {
      def map[A, B](pa: PioParse[A])(f: A => B): PioParse[B] =
        PioParse(pa.run.andThen(_.map(f)))
    }

  def enum[A](dictionary: (String, A)*): PioParse[A] =
    PioParse(dictionary.toMap.lift)

  /** Builds a PioParse for an `Enumerated` instance, assuming that the enum
    * tags will be used as the lookup keys.  In other words, this is an option
    * for enumerations whose OCS2 export happen to match the new model enum
    * tags.
    */
  def enumFromTag[A](as: List[A])(implicit ev: Enumerated[A]): PioParse[A] =
    PioParse(as.map(a => ev.tag(a) -> a).toMap.lift)

  // ********  Primitives

  val bigDecimal: PioParse[BigDecimal] =
    PioParse(_.parseBigDecimalOption)

  val boolean: PioParse[Boolean] =
    PioParse(_.parseBooleanOption)

  val double: PioParse[Double] =
    PioParse(_.parseDoubleOption)

  val short: PioParse[Short] =
    PioParse(_.parseShortOption)

  val int: PioParse[Int] =
    PioParse(_.parseIntOption)

  val positiveInt: PioParse[Int] =
    PioParse(_.parseIntOption.filter(_ > 0))

  val positiveShort: PioParse[Short] =
    PioParse(_.parseShortOption.filter(_ > 0))

  val long: PioParse[Long] =
    PioParse(_.parseLongOption)

  val string: PioParse[String] =
    PioParse(_.pure[Option])

  // ********  Java

  val instant: PioParse[Instant] =
    long.map(Instant.ofEpochMilli)

  val seconds: PioParse[Duration] =
    double.map(d => Duration.ofMillis((d * 1000).round))
}
