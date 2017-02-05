package gem.ocs2.pio

import java.time.{Duration, Instant}

import scalaz._
import Scalaz._

object PioParse {
  implicit val FunctorPioParse = new Functor[PioParse] {
    def map[A, B](pa: PioParse[A])(f: A => B): PioParse[B] =
      pa andThen (_.map(f))
  }

  def enum[A](dictionary: (String, A)*): PioParse[A] =
    dictionary.toMap.lift

  // ********  Primitives

  val boolean: PioParse[Boolean] =
    _.parseBoolean.toOption

  val double: PioParse[Double] =
    _.parseDouble.toOption

  val int: PioParse[Int] =
    _.parseInt.toOption

  val long: PioParse[Long] =
    _.parseLong.toOption

  val string: PioParse[String] =
    _.pure[Option]


  // ********  Java

  val instant: PioParse[Instant] =
    long.map(Instant.ofEpochMilli)

  val seconds: PioParse[Duration] =
    double.map { d => Duration.ofMillis((d * 1000).round) }
}
