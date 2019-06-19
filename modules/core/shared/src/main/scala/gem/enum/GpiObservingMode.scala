// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.Eval
import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated
import gsp.math.MagnitudeValue

/**
 * Enumerated type for GPI ObservingMode.
 * @group Enumerations (Generated)
 */
sealed abstract class GpiObservingMode(
  val tag: String,
  val shortName: String,
  val longName: String,
  val filter: Option[GpiFilter],
  val filterIterable: Boolean,
  val apodizer: Option[GpiApodizer],
  val fpm: Option[GpiFPM],
  val lyot: Option[GpiLyot],
  val brightLimitPrism: Option[MagnitudeValue],
  val brightLimitWollaston: Option[MagnitudeValue],
  val correspondingHMode: Eval[GpiObservingMode],
  val obsolete: Boolean
) extends Product with Serializable

object GpiObservingMode {

  /** @group Constructors */ case object CORON_Y_BAND extends GpiObservingMode("CORON_Y_BAND", "Coronograph Y-band", "Coronograph Y-band", Some(GpiFilter.Y), false, Some(GpiApodizer.APOD_Y), Some(GpiFPM.FPM_Y), Some(GpiLyot.LYOT_080m12_03), Some(MagnitudeValue(50)), Some(MagnitudeValue(300)), cats.Eval.later(GpiObservingMode.unsafeFromTag("CORON_H_BAND")), false)
  /** @group Constructors */ case object CORON_J_BAND extends GpiObservingMode("CORON_J_BAND", "Coronograph J-band", "Coronograph J-band", Some(GpiFilter.J), false, Some(GpiApodizer.APOD_J), Some(GpiFPM.FPM_J), Some(GpiLyot.LYOT_080m12_04), Some(MagnitudeValue(50)), Some(MagnitudeValue(300)), cats.Eval.later(GpiObservingMode.unsafeFromTag("CORON_H_BAND")), false)
  /** @group Constructors */ case object CORON_H_BAND extends GpiObservingMode("CORON_H_BAND", "Coronograph H-band", "Coronograph H-band", Some(GpiFilter.H), false, Some(GpiApodizer.APOD_H), Some(GpiFPM.FPM_H), Some(GpiLyot.LYOT_080m12_04), Some(MagnitudeValue(50)), Some(MagnitudeValue(300)), cats.Eval.later(GpiObservingMode.unsafeFromTag("CORON_H_BAND")), false)
  /** @group Constructors */ case object CORON_K1_BAND extends GpiObservingMode("CORON_K1_BAND", "Coronograph K1-band", "Coronograph K1-band", Some(GpiFilter.K1), false, Some(GpiApodizer.APOD_K1), Some(GpiFPM.FPM_K1), Some(GpiLyot.LYOT_080m12_06_03), Some(MagnitudeValue(50)), Some(MagnitudeValue(300)), cats.Eval.later(GpiObservingMode.unsafeFromTag("CORON_H_BAND")), false)
  /** @group Constructors */ case object CORON_K2_BAND extends GpiObservingMode("CORON_K2_BAND", "Coronograph K2-band", "Coronograph K2-band", Some(GpiFilter.K2), false, Some(GpiApodizer.APOD_K2), Some(GpiFPM.FPM_K1), Some(GpiLyot.LYOT_080m12_07), Some(MagnitudeValue(50)), Some(MagnitudeValue(300)), cats.Eval.later(GpiObservingMode.unsafeFromTag("CORON_H_BAND")), false)
  /** @group Constructors */ case object H_STAR extends GpiObservingMode("H_STAR", "H_STAR", "H_STAR", Some(GpiFilter.H), false, Some(GpiApodizer.APOD_STAR), Some(GpiFPM.FPM_H), Some(GpiLyot.LYOT_080m12_03), Option.empty[MagnitudeValue], Option.empty[MagnitudeValue], cats.Eval.later(GpiObservingMode.unsafeFromTag("H_STAR")), true)
  /** @group Constructors */ case object H_LIWA extends GpiObservingMode("H_LIWA", "H_LIWA", "H_LIWA", Some(GpiFilter.H), false, Some(GpiApodizer.APOD_HL), Some(GpiFPM.FPM_K1), Some(GpiLyot.LYOT_080m12_04), Option.empty[MagnitudeValue], Option.empty[MagnitudeValue], cats.Eval.later(GpiObservingMode.unsafeFromTag("H_LIWA")), false)
  /** @group Constructors */ case object DIRECT_Y_BAND extends GpiObservingMode("DIRECT_Y_BAND", "Y direct", "Y direct", Some(GpiFilter.Y), true, Some(GpiApodizer.CLEAR), Some(GpiFPM.SCIENCE), Some(GpiLyot.OPEN), Some(MagnitudeValue(550)), Some(MagnitudeValue(750)), cats.Eval.later(GpiObservingMode.unsafeFromTag("DIRECT_H_BAND")), false)
  /** @group Constructors */ case object DIRECT_J_BAND extends GpiObservingMode("DIRECT_J_BAND", "J direct", "J direct", Some(GpiFilter.J), true, Some(GpiApodizer.CLEAR), Some(GpiFPM.SCIENCE), Some(GpiLyot.OPEN), Some(MagnitudeValue(550)), Some(MagnitudeValue(750)), cats.Eval.later(GpiObservingMode.unsafeFromTag("DIRECT_H_BAND")), false)
  /** @group Constructors */ case object DIRECT_H_BAND extends GpiObservingMode("DIRECT_H_BAND", "H direct", "H direct", Some(GpiFilter.H), true, Some(GpiApodizer.CLEAR), Some(GpiFPM.SCIENCE), Some(GpiLyot.OPEN), Some(MagnitudeValue(550)), Some(MagnitudeValue(750)), cats.Eval.later(GpiObservingMode.unsafeFromTag("DIRECT_H_BAND")), false)
  /** @group Constructors */ case object DIRECT_K1_BAND extends GpiObservingMode("DIRECT_K1_BAND", "K1 direct", "K1 direct", Some(GpiFilter.K1), true, Some(GpiApodizer.CLEAR), Some(GpiFPM.SCIENCE), Some(GpiLyot.OPEN), Some(MagnitudeValue(550)), Some(MagnitudeValue(750)), cats.Eval.later(GpiObservingMode.unsafeFromTag("DIRECT_H_BAND")), false)
  /** @group Constructors */ case object DIRECT_K2_BAND extends GpiObservingMode("DIRECT_K2_BAND", "K2 direct", "K2 direct", Some(GpiFilter.K2), true, Some(GpiApodizer.CLEAR), Some(GpiFPM.SCIENCE), Some(GpiLyot.OPEN), Some(MagnitudeValue(550)), Some(MagnitudeValue(750)), cats.Eval.later(GpiObservingMode.unsafeFromTag("DIRECT_H_BAND")), false)
  /** @group Constructors */ case object NRM_Y extends GpiObservingMode("NRM_Y", "Non Redundant Mask Y", "Non Redundant Mask Y", Some(GpiFilter.Y), true, Some(GpiApodizer.NRM), Some(GpiFPM.SCIENCE), Some(GpiLyot.OPEN), Option.empty[MagnitudeValue], Option.empty[MagnitudeValue], cats.Eval.later(GpiObservingMode.unsafeFromTag("NRM_H")), false)
  /** @group Constructors */ case object NRM_J extends GpiObservingMode("NRM_J", "Non Redundant Mask J", "Non Redundant Mask J", Some(GpiFilter.J), true, Some(GpiApodizer.NRM), Some(GpiFPM.SCIENCE), Some(GpiLyot.OPEN), Option.empty[MagnitudeValue], Option.empty[MagnitudeValue], cats.Eval.later(GpiObservingMode.unsafeFromTag("NRM_H")), false)
  /** @group Constructors */ case object NRM_H extends GpiObservingMode("NRM_H", "Non Redundant Mask H", "Non Redundant Mask H", Some(GpiFilter.H), true, Some(GpiApodizer.NRM), Some(GpiFPM.SCIENCE), Some(GpiLyot.OPEN), Option.empty[MagnitudeValue], Option.empty[MagnitudeValue], cats.Eval.later(GpiObservingMode.unsafeFromTag("NRM_H")), false)
  /** @group Constructors */ case object NRM_K1 extends GpiObservingMode("NRM_K1", "Non Redundant Mask K1", "Non Redundant Mask K1", Some(GpiFilter.K1), true, Some(GpiApodizer.NRM), Some(GpiFPM.SCIENCE), Some(GpiLyot.OPEN), Option.empty[MagnitudeValue], Option.empty[MagnitudeValue], cats.Eval.later(GpiObservingMode.unsafeFromTag("NRM_H")), false)
  /** @group Constructors */ case object NRM_K2 extends GpiObservingMode("NRM_K2", "Non Redundant Mask K2", "Non Redundant Mask K2", Some(GpiFilter.K2), true, Some(GpiApodizer.NRM), Some(GpiFPM.SCIENCE), Some(GpiLyot.OPEN), Option.empty[MagnitudeValue], Option.empty[MagnitudeValue], cats.Eval.later(GpiObservingMode.unsafeFromTag("NRM_H")), false)
  /** @group Constructors */ case object DARK extends GpiObservingMode("DARK", "Dark", "Dark", Some(GpiFilter.H), false, Some(GpiApodizer.APOD_H), Some(GpiFPM.FPM_H), Some(GpiLyot.BLANK), Some(MagnitudeValue(50)), Some(MagnitudeValue(300)), cats.Eval.later(GpiObservingMode.unsafeFromTag("DARK")), false)
  /** @group Constructors */ case object UNBLOCKED_Y extends GpiObservingMode("UNBLOCKED_Y", "Y Unblocked", "Y Unblocked", Some(GpiFilter.Y), false, Some(GpiApodizer.APOD_Y), Some(GpiFPM.SCIENCE), Some(GpiLyot.LYOT_080m12_03), Some(MagnitudeValue(400)), Some(MagnitudeValue(650)), cats.Eval.later(GpiObservingMode.unsafeFromTag("UNBLOCKED_Y")), false)
  /** @group Constructors */ case object UNBLOCKED_J extends GpiObservingMode("UNBLOCKED_J", "J Unblocked", "J Unblocked", Some(GpiFilter.J), false, Some(GpiApodizer.APOD_J), Some(GpiFPM.SCIENCE), Some(GpiLyot.LYOT_080m12_04), Some(MagnitudeValue(400)), Some(MagnitudeValue(650)), cats.Eval.later(GpiObservingMode.unsafeFromTag("UNBLOCKED_J")), false)
  /** @group Constructors */ case object UNBLOCKED_H extends GpiObservingMode("UNBLOCKED_H", "H Unblocked", "H Unblocked", Some(GpiFilter.H), false, Some(GpiApodizer.APOD_H), Some(GpiFPM.SCIENCE), Some(GpiLyot.LYOT_080m12_04), Some(MagnitudeValue(400)), Some(MagnitudeValue(650)), cats.Eval.later(GpiObservingMode.unsafeFromTag("UNBLOCKED_H")), false)
  /** @group Constructors */ case object UNBLOCKED_K1 extends GpiObservingMode("UNBLOCKED_K1", "K1 Unblocked", "K1 Unblocked", Some(GpiFilter.K1), false, Some(GpiApodizer.APOD_K1), Some(GpiFPM.SCIENCE), Some(GpiLyot.LYOT_080m12_06_03), Some(MagnitudeValue(400)), Some(MagnitudeValue(650)), cats.Eval.later(GpiObservingMode.unsafeFromTag("UNBLOCKED_K1")), false)
  /** @group Constructors */ case object UNBLOCKED_K2 extends GpiObservingMode("UNBLOCKED_K2", "K2 Unblocked", "K2 Unblocked", Some(GpiFilter.K2), false, Some(GpiApodizer.APOD_K2), Some(GpiFPM.SCIENCE), Some(GpiLyot.LYOT_080m12_07), Some(MagnitudeValue(400)), Some(MagnitudeValue(650)), cats.Eval.later(GpiObservingMode.unsafeFromTag("UNBLOCKED_K2")), false)
  /** @group Constructors */ case object NONSTANDARD extends GpiObservingMode("NONSTANDARD", "Nonstandard", "Nonstandard", Option.empty[GpiFilter], false, Option.empty[GpiApodizer], Option.empty[GpiFPM], Option.empty[GpiLyot], Option.empty[MagnitudeValue], Option.empty[MagnitudeValue], cats.Eval.later(GpiObservingMode.unsafeFromTag("NONSTANDARD")), false)

  /** All members of GpiObservingMode, in canonical order. */
  val all: List[GpiObservingMode] =
    List(CORON_Y_BAND, CORON_J_BAND, CORON_H_BAND, CORON_K1_BAND, CORON_K2_BAND, H_STAR, H_LIWA, DIRECT_Y_BAND, DIRECT_J_BAND, DIRECT_H_BAND, DIRECT_K1_BAND, DIRECT_K2_BAND, NRM_Y, NRM_J, NRM_H, NRM_K1, NRM_K2, DARK, UNBLOCKED_Y, UNBLOCKED_J, UNBLOCKED_H, UNBLOCKED_K1, UNBLOCKED_K2, NONSTANDARD)

  /** Select the member of GpiObservingMode with the given tag, if any. */
  def fromTag(s: String): Option[GpiObservingMode] =
    all.find(_.tag === s)

  /** Select the member of GpiObservingMode with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GpiObservingMode =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GpiObservingMode: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GpiObservingModeEnumerated: Enumerated[GpiObservingMode] =
    new Enumerated[GpiObservingMode] {
      def all = GpiObservingMode.all
      def tag(a: GpiObservingMode) = a.tag
      override def unsafeFromTag(s: String): GpiObservingMode =
        GpiObservingMode.unsafeFromTag(s)
    }

}