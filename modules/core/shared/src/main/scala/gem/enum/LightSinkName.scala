// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for SF Sink names.
 * @group Enumerations (Generated)
 */
sealed abstract class LightSinkName(
  val tag: String,
  val name: String
) extends Product with Serializable

object LightSinkName {

  /** @group Constructors */ case object Gmos extends LightSinkName("Gmos", "gmos")
  /** @group Constructors */ case object Niri_f6 extends LightSinkName("Niri_f6", "nirif6p")
  /** @group Constructors */ case object Niri_f14 extends LightSinkName("Niri_f14", "nirif14p")
  /** @group Constructors */ case object Niri_f32 extends LightSinkName("Niri_f32", "nirif32p")
  /** @group Constructors */ case object Ac extends LightSinkName("Ac", "ac")
  /** @group Constructors */ case object Hr extends LightSinkName("Hr", "hr")
  /** @group Constructors */ case object Nifs extends LightSinkName("Nifs", "nifs")
  /** @group Constructors */ case object Gmos_Ifu extends LightSinkName("Gmos_Ifu", "gmosifu")
  /** @group Constructors */ case object Gnirs extends LightSinkName("Gnirs", "gnirs")
  /** @group Constructors */ case object Visitor extends LightSinkName("Visitor", "visitor")
  /** @group Constructors */ case object F2 extends LightSinkName("F2", "f2")
  /** @group Constructors */ case object Gsaoi extends LightSinkName("Gsaoi", "gsaoi")
  /** @group Constructors */ case object Phoenix extends LightSinkName("Phoenix", "phoenix")
  /** @group Constructors */ case object Gpi extends LightSinkName("Gpi", "gpi")
  /** @group Constructors */ case object Ghost extends LightSinkName("Ghost", "ghost")

  /** All members of LightSinkName, in canonical order. */
  val all: List[LightSinkName] =
    List(Gmos, Niri_f6, Niri_f14, Niri_f32, Ac, Hr, Nifs, Gmos_Ifu, Gnirs, Visitor, F2, Gsaoi, Phoenix, Gpi, Ghost)

  /** Select the member of LightSinkName with the given tag, if any. */
  def fromTag(s: String): Option[LightSinkName] =
    all.find(_.tag === s)

  /** Select the member of LightSinkName with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): LightSinkName =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"LightSinkName: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val LightSinkNameEnumerated: Enumerated[LightSinkName] =
    new Enumerated[LightSinkName] {
      def all = LightSinkName.all
      def tag(a: LightSinkName) = a.tag
      override def unsafeFromTag(s: String): LightSinkName =
        LightSinkName.unsafeFromTag(s)
    }

}