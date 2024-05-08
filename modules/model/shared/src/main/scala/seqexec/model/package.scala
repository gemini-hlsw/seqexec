// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec

import java.util.UUID

import cats._
import cats.syntax.all._
import seqexec.model.enum._
import squants.time.Time
import squants.time.TimeUnit

package model {
  final case class QueueId(self: UUID)  extends AnyVal
  final case class ClientId(self: UUID) extends AnyVal
}

package object model {
  type ParamName       = String
  type ParamValue      = String
  type Parameters      = Map[ParamName, ParamValue]
  type StepConfig      = Map[SystemName, Parameters]
  type StepId          = Int
  type ObservationName = String
  type TargetName      = String

  implicit val queueIdEq: Eq[QueueId]                        = Eq.by(x => x.self)
  implicit val queueIdShow: Show[QueueId]                    = Show.fromToString
  implicit val queueIdOrder: Order[QueueId]                  =
    Order.by(_.self)
  implicit val queueIdOrdering: scala.math.Ordering[QueueId] =
    queueIdOrder.toOrdering

  implicit val stEq: Eq[StepConfig]                            = Eq.fromUniversalEquals
  implicit val clientIdEq: Eq[ClientId]                        = Eq.by(x => x.self)
  implicit val clientIdShow: Show[ClientId]                    = Show.fromToString
  implicit val clientIdOrder: Order[ClientId]                  =
    Order.by(_.self)
  implicit val clientIdOrdering: scala.math.Ordering[ClientId] =
    clientIdOrder.toOrdering
  val UnknownTargetName                                        = "None"

  val CalibrationQueueName: String = "Calibration Queue"
  val CalibrationQueueId: QueueId  =
    QueueId(UUID.fromString("7156fa7e-48a6-49d1-a267-dbf3bbaa7577"))

  implicit val timeUnit: Eq[TimeUnit] =
    Eq.by(_.symbol)

  implicit val timeEq: Eq[Time] =
    Eq.by(_.toMilliseconds)

  implicit class InstrumentOps(val i: Instrument) extends AnyVal {
    def hasOI: Boolean = i match {
      case Instrument.F2    => true
      case Instrument.GmosS => true
      case Instrument.GmosN => true
      case Instrument.Nifs  => true
      case Instrument.Niri  => true
      case Instrument.Gnirs => true
      case Instrument.Gsaoi => false
      case Instrument.Gpi   => true
      case Instrument.Ghost => false
      case _                => false
    }
  }

  trait GhostTargetName {
    def targetModeFromNames(
      srifu1: Option[String],
      srifu2: Option[String],
      hrifu1: Option[String],
      hrifu2: Option[String]
    ): Option[String] = (srifu1, srifu2, hrifu1, hrifu2) match {
      case (Some(_), Some("Sky"), None, None)       => "SRIFU1 Target, SRIFU2 Sky".some
      case (Some("Sky"), Some(_), None, None)       => "SRIFU1 Sky, SRIFU2 Target".some
      case (Some(_), Some(_), None, None)           => "Dual Target".some
      case (Some(_), None, None, None)              => "Single Target".some
      case (None, None, Some(_), Some("Sky"))       => "HRIFU Target, Sky".some
      case (None, None, Some(_), Some("Sky (PRV)")) => "HRIFU Target, Sky (PRV)".some
      case _                                        => None
    }

    private def ellipsis(text: String, max: Int): String =
      if (text.length <= max) {
        text
      } else {
        text.substring(0, max - 3) + "..."
      }

    def targetNameFromNames(
      srifu1: Option[String],
      srifu2: Option[String],
      hrifu1: Option[String],
      hrifu2: Option[String]
    ): Option[String] = (srifu1, srifu2, hrifu1, hrifu2) match {
      case (Some(t), Some("Sky"), None, None)       => t.some
      case (Some("Sky"), Some(t), None, None)       => t.some
      // max value length is 70 so we have to subtract 3 for the ellipsis
      case (Some(t1), Some(t2), None, None)         =>
        s"${ellipsis(t1, 35 - 3)}, ${ellipsis(t2, 35 - 3)}".some
      case (Some(t), None, None, None)              => t.some
      case (None, None, Some(t), Some("Sky"))       => t.some
      case (None, None, Some(t), Some("Sky (PRV)")) => t.some
      case _                                        => None
    }
  }

}
