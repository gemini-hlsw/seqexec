// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.Show
import cats.data.NonEmptyList
import gem.util.Enumerated

/** A Seqexec resource represents any system that can be only used by one single agent. */
sealed abstract class Resource(val ordinal: Int, val label: String)
  extends Product with Serializable {
  def isInstrument: Boolean = false
}

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
  implicit val show: Show[Resource] =
    Show.show(_.label)

  val common: List[Resource] = List(TCS, Gcal)

  /** @group Typeclass Instances */
  implicit val ResourceEnumerated: Enumerated[Resource] =
    Enumerated.of(Instrument.allResources.head, Instrument.allResources.tail: _*)
}

sealed abstract class Instrument(ordinal: Int, label: String)
  extends Resource(ordinal, label) {
  override def isInstrument: Boolean = true
}

object Instrument {

  case object F2    extends Instrument(11, "Flamingos2")
  case object Ghost extends Instrument(12, "GHOST")
  case object GmosS extends Instrument(13, "GMOS-S")
  case object GmosN extends Instrument(14, "GMOS-N")
  case object Gnirs extends Instrument(15, "GNIRS")
  case object Gpi   extends Instrument(16, "GPI")
  case object Gsaoi extends Instrument(17, "GSAOI")
  case object Niri  extends Instrument(18, "NIRI")
  case object Nifs  extends Instrument(19, "NIFS")

  implicit val show: Show[Instrument] =
    Show.show(_.label)

  val gsInstruments: NonEmptyList[Instrument] =
    NonEmptyList.of(F2, Ghost, GmosS, Gpi, Gsaoi)

  val gnInstruments: NonEmptyList[Instrument] =
    NonEmptyList.of(GmosN, Gnirs, Niri, Nifs)

  val all: NonEmptyList[Instrument] =
    gsInstruments.concatNel(gnInstruments)

  val allResources: NonEmptyList[Resource] =
    NonEmptyList.of(Resource.P1, Resource.OI, Resource.TCS, Resource.Gcal, Resource.Gems, Resource.Altair) ::: Instrument.all

  /** @group Typeclass Instances */
  implicit val InstrumentEnumerated: Enumerated[Instrument] =
    Enumerated.of(all.head, all.tail: _*)
}
