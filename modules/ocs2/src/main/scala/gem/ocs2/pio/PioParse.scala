package gem.ocs2.pio

import java.time.{Duration, Instant}

import scalaz._
import Scalaz._

final case class PioParse[A](run: String => Option[A]) {
  def apply(s: String): Option[A] = run(s)
}

object PioParse {
  implicit val FunctorPioParse = new Functor[PioParse] {
    def map[A, B](pa: PioParse[A])(f: A => B): PioParse[B] =
      PioParse(pa.run andThen (_.map(f)))
  }

  def enum[A](dictionary: (String, A)*): PioParse[A] =
    PioParse(dictionary.toMap.lift)

  // ********  Primitives

  val boolean: PioParse[Boolean] =
    PioParse(_.parseBoolean.toOption)

  val double: PioParse[Double] =
    PioParse(_.parseDouble.toOption)

  val int: PioParse[Int] =
    PioParse(_.parseInt.toOption)

  val long: PioParse[Long] =
    PioParse(_.parseLong.toOption)

  val string: PioParse[String] =
    PioParse(_.pure[Option])


  // ********  Java

  val instant: PioParse[Instant] =
    long.map(Instant.ofEpochMilli)

  val seconds: PioParse[Duration] =
    double.map { d => Duration.ofMillis((d * 1000).round) }
}
