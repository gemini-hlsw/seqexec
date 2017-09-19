// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for calibration unit filter.
 * @group Enumerations (Generated)
 */
sealed abstract class GcalFilter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable

object GcalFilter {

  /** @group Constructors */ case object None extends GcalFilter("None", "none", "none", false)
  /** @group Constructors */ case object Gmos extends GcalFilter("Gmos", "GMOS balance", "GMOS balance", false)
  /** @group Constructors */ case object Hros extends GcalFilter("Hros", "HROS balance", "HROS balance", true)
  /** @group Constructors */ case object Nir extends GcalFilter("Nir", "NIR balance", "NIR balance", false)
  /** @group Constructors */ case object Nd10 extends GcalFilter("Nd10", "ND1.0", "ND1.0", false)
  /** @group Constructors */ case object Nd16 extends GcalFilter("Nd16", "ND1.6", "ND1.6", true)
  /** @group Constructors */ case object Nd20 extends GcalFilter("Nd20", "ND2.0", "ND2.0", false)
  /** @group Constructors */ case object Nd30 extends GcalFilter("Nd30", "ND3.0", "ND3.0", false)
  /** @group Constructors */ case object Nd40 extends GcalFilter("Nd40", "ND4.0", "ND4.0", false)
  /** @group Constructors */ case object Nd45 extends GcalFilter("Nd45", "ND4-5", "ND4-5", false)
  /** @group Constructors */ case object Nd50 extends GcalFilter("Nd50", "ND5.0", "ND5.0", true)

  /** All members of GcalFilter, in canonical order. */
  val all: List[GcalFilter] =
    List(None, Gmos, Hros, Nir, Nd10, Nd16, Nd20, Nd30, Nd40, Nd45, Nd50)

  /** Select the member of GcalFilter with the given tag, if any. */
  def fromTag(s: String): Option[GcalFilter] =
    all.find(_.tag === s)

  /** Select the member of GcalFilter with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GcalFilter =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GcalFilterEnumerated: Enumerated[GcalFilter] =
    new Enumerated[GcalFilter] {
      def all = GcalFilter.all
      def tag(a: GcalFilter) = a.tag
    }

}