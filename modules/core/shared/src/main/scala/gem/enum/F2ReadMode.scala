// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for Flamingos2 read modes.
 * @group Enumerations (Generated)
 */
sealed abstract class F2ReadMode(
  val tag: String,
  val shortName: String,
  val longName: String,
  val description: String,
  val minimumExposureTime: java.time.Duration,
  val recommendedExposureTime: java.time.Duration,
  val readoutTime: java.time.Duration,
  val readCount: Int,
  val readNoise: Double
) extends Product with Serializable

object F2ReadMode {

  /** @group Constructors */ case object Bright extends F2ReadMode("Bright", "bright", "Bright Object", "Strong Source", java.time.Duration.ofMillis(1500), java.time.Duration.ofMillis(5000), java.time.Duration.ofMillis(8000), 1, 11.7)
  /** @group Constructors */ case object Medium extends F2ReadMode("Medium", "medium", "Medium Object", "Medium Source", java.time.Duration.ofMillis(6000), java.time.Duration.ofMillis(21000), java.time.Duration.ofMillis(14000), 4, 6.0)
  /** @group Constructors */ case object Faint extends F2ReadMode("Faint", "faint", "Faint Object", "Weak Source", java.time.Duration.ofMillis(12000), java.time.Duration.ofMillis(85000), java.time.Duration.ofMillis(20000), 8, 5.0)

  /** All members of F2ReadMode, in canonical order. */
  val all: List[F2ReadMode] =
    List(Bright, Medium, Faint)

  /** Select the member of F2ReadMode with the given tag, if any. */
  def fromTag(s: String): Option[F2ReadMode] =
    all.find(_.tag === s)

  /** Select the member of F2ReadMode with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): F2ReadMode =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val F2ReadModeEnumerated: Enumerated[F2ReadMode] =
    new Enumerated[F2ReadMode] {
      def all = F2ReadMode.all
      def tag(a: F2ReadMode) = a.tag
    }

}