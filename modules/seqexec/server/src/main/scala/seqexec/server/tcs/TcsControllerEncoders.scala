// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import seqexec.model.enum.{ComaOption, MountGuideOption}
import seqexec.model.M2GuideConfig
import seqexec.model.M1GuideConfig
import seqexec.server.EpicsCodex.EncodeEpicsValue
import seqexec.server.tcs.TcsController._

trait TcsControllerEncoders {
  // Encoders
  implicit val encodeMountGuideConfig: EncodeEpicsValue[MountGuideOption, String] =
    EncodeEpicsValue{
      case MountGuideOption.MountGuideOn  => "on"
      case MountGuideOption.MountGuideOff => "off"
    }

  implicit val encodeM1GuideConfig: EncodeEpicsValue[M1GuideConfig, String] =
    EncodeEpicsValue {
      case M1GuideConfig.M1GuideOn(_) => "on"
      case M1GuideConfig.M1GuideOff   => "off"
    }

  val encodeM2Guide: EncodeEpicsValue[M2GuideConfig, String] =
    EncodeEpicsValue {
      case M2GuideConfig.M2GuideOn(_, _) => "on"
      case M2GuideConfig.M2GuideOff      => "off"
    }

  val encodeM2Coma: EncodeEpicsValue[M2GuideConfig, String] =
    EncodeEpicsValue {
      case M2GuideConfig.M2GuideOn(ComaOption.ComaOn, _) => "on"
      case _                                             => "off"
    }

  val encodeM2GuideReset: EncodeEpicsValue[M2GuideConfig, String] =
    EncodeEpicsValue {
      case M2GuideConfig.M2GuideOn(_, _) => "off"
      case M2GuideConfig.M2GuideOff      => "on"
    }

  implicit val encodeNodChopOption: EncodeEpicsValue[NodChopTrackingOption, String] =
    EncodeEpicsValue {
      case NodChopTrackingOption.NodChopTrackingOn  => "On"
      case NodChopTrackingOption.NodChopTrackingOff => "Off"
    }

  implicit val encodeFollowOption: EncodeEpicsValue[FollowOption, String] =
    EncodeEpicsValue {
      case FollowOption.FollowOn  => "On"
      case FollowOption.FollowOff => "Off"
    }

  implicit val encodeHrwfsPickupPosition: EncodeEpicsValue[HrwfsPickupPosition, String] =
    EncodeEpicsValue{
      case HrwfsPickupPosition.IN     => "IN"
      case HrwfsPickupPosition.OUT    => "OUT"
      case HrwfsPickupPosition.Parked => "park-pos."
    }

}
