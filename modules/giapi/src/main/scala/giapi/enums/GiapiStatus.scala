// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.enums

import cats.syntax.eq._
import lucuma.core.util.Enumerated
import lucuma.core.enums.Instrument

/**
 * Enumerated type for Giapi Status.
 * @group Enumerations (Generated)
 */
sealed abstract class GiapiStatus(
  val tag:        String,
  val instrument: Instrument,
  val statusType: GiapiType,
  val statusItem: String
) extends Product
    with Serializable

object GiapiStatus {

  /** @group Constructors */
  case object GpiGuiding
      extends GiapiStatus("GpiGuiding", Instrument.Gpi, GiapiType.Int, "gpi:guiding")

  /** @group Constructors */
  case object GpiAlignAndCalibState
      extends GiapiStatus("GpiAlignAndCalibState",
                          Instrument.Gpi,
                          GiapiType.Int,
                          "gpi:alignCalibMode"
      )

  /** All members of GiapiStatus, in canonical order. */
  val all: List[GiapiStatus] =
    List(GpiGuiding, GpiAlignAndCalibState)

  /** Select the member of GiapiStatus with the given tag, if any. */
  def fromTag(s: String): Option[GiapiStatus] =
    all.find(_.tag === s)

  /** Select the member of GiapiStatus with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GiapiStatus =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GiapiStatus: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GiapiStatusEnumerated: Enumerated[GiapiStatus] =
    new Enumerated[GiapiStatus] {
      def all = GiapiStatus.all
      def tag(a: GiapiStatus) = a.tag
      override def unsafeFromTag(s: String): GiapiStatus =
        GiapiStatus.unsafeFromTag(s)
    }

}
