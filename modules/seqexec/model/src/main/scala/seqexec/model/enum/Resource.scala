// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.{ Eq, Order, Show }
import cats.data.NonEmptyList
import cats.implicits._

/** A Seqexec resource represents any system that can be only used by one single agent. */
sealed abstract class Resource(val ordinal: Int, val label: String)
  extends Product with Serializable

object Resource {

  case object P1     extends Resource(1, "P1")
  case object OI     extends Resource(2, "OI")
  case object TCS    extends Resource(3, "TCS")
  case object Gcal   extends Resource(4, "Gcal")
  case object Gems   extends Resource(5, "Gems")
  case object Altair extends Resource(6, "Altair")

  // Mount and science fold cannot be controlled independently. Maybe in the future.
  // For now, I replaced them with TCS
  //  case object Mount extends Resource
  //  case object ScienceFold extends Resource

  implicit val order: Order[Resource] =
    Order.by(_.ordinal)

  implicit val show: Show[Resource] =
    Show.show(_.label)

  implicit val ordering: Ordering[Resource] =
    order.toOrdering
}

sealed abstract class Instrument(ordinal: Int, label: String)
  extends Resource(ordinal, label)

object Instrument {

  case object F2    extends Instrument(11, "Flamingos2")
  case object GHOST extends Instrument(12, "GHOST")
  case object GmosS extends Instrument(13, "GMOS-S")
  case object GmosN extends Instrument(14, "GMOS-N")
  case object GNIRS extends Instrument(15, "GNIRS")
  case object GPI   extends Instrument(16, "GPI")
  case object GSAOI extends Instrument(17, "GSAOI")
  case object NIRI  extends Instrument(18, "NIRI")
  case object NIFS  extends Instrument(19, "NIFS")

  implicit val equal: Eq[Instrument] =
    Eq.fromUniversalEquals

  implicit val show: Show[Instrument] =
    Show.show(_.label)

  val gsInstruments: NonEmptyList[Instrument] =
    NonEmptyList.of(F2, GHOST, GmosS, GPI, GSAOI)

  val gnInstruments: NonEmptyList[Instrument] =
    NonEmptyList.of(GmosN, GNIRS, NIRI, NIFS)

  val all: NonEmptyList[Instrument] =
    gsInstruments.concatNel(gnInstruments)

}
