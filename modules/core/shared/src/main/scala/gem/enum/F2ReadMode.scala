// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated
import scala.concurrent.duration._

/**
 * Enumerated type for Flamingos2 read modes.
 * @group Enumerations (Generated)
 */
sealed abstract class F2ReadMode(
  val tag: String,
  val shortName: String,
  val longName: String,
  val description: String,
  val minimumExposureTime: FiniteDuration,
  val recommendedExposureTime: FiniteDuration,
  val readoutTime: FiniteDuration,
  val readCount: Int,
  val readNoise: Double
) extends Product with Serializable

object F2ReadMode {

  /** @group Constructors */ case object Bright extends F2ReadMode("Bright", "bright", "Bright Object", "Strong Source", 1500.millis, 5000.millis, 8000.millis, 1, 11.7)
  /** @group Constructors */ case object Medium extends F2ReadMode("Medium", "medium", "Medium Object", "Medium Source", 6000.millis, 21000.millis, 14000.millis, 4, 6.0)
  /** @group Constructors */ case object Faint extends F2ReadMode("Faint", "faint", "Faint Object", "Weak Source", 12000.millis, 85000.millis, 20000.millis, 8, 5.0)

  /** All members of F2ReadMode, in canonical order. */
  val all: List[F2ReadMode] =
    List(Bright, Medium, Faint)

  /** Select the member of F2ReadMode with the given tag, if any. */
  def fromTag(s: String): Option[F2ReadMode] =
    all.find(_.tag === s)

  /** Select the member of F2ReadMode with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): F2ReadMode =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"F2ReadMode: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val F2ReadModeEnumerated: Enumerated[F2ReadMode] =
    new Enumerated[F2ReadMode] {
      def all = F2ReadMode.all
      def tag(a: F2ReadMode) = a.tag
      override def unsafeFromTag(s: String): F2ReadMode =
        F2ReadMode.unsafeFromTag(s)
    }

}