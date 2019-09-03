// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import cats.{Eq, Show}
import cats.implicits._
import edu.gemini.spModel.gemini.calunit.CalUnitParams.Shutter

trait GcalController[F[_]] {

  import GcalController._

  def applyConfig(config: GcalConfig): F[Unit]

}

object GcalController {
  sealed trait LampState extends Product with Serializable

  object LampState {

    case object Off extends LampState

    case object On extends LampState

    implicit val eq: Eq[LampState] =
      Eq.fromUniversalEquals

  }

  final case class ArLampState(self: LampState)

  object ArLampState {
    implicit val eq: Eq[ArLampState] =
      Eq[LampState].contramap(_.self)
  }

  final case class CuArLampState(self: LampState)

  object CuArLampState {
    implicit val eq: Eq[CuArLampState] =
      Eq[LampState].contramap(_.self)
  }

  final case class QHLampState(self: LampState)

  object QHLampState {
    implicit val eq: Eq[QHLampState] =
      Eq[LampState].contramap(_.self)
  }

  final case class ThArLampState(self: LampState)

  object ThArLampState {
    implicit val eq: Eq[ThArLampState] =
      Eq[LampState].contramap(_.self)
  }

  final case class XeLampState(self: LampState)

  object XeLampState {
    implicit val eq: Eq[XeLampState] =
      Eq[LampState].contramap(_.self)
  }

  final case class IrLampState(self: LampState)

  object IrLampState {
    implicit val eq: Eq[IrLampState] =
      Eq[LampState].contramap(_.self)
  }

  type Shutter = edu.gemini.spModel.gemini.calunit.CalUnitParams.Shutter

  type Filter = edu.gemini.spModel.gemini.calunit.CalUnitParams.Filter

  type Diffuser = edu.gemini.spModel.gemini.calunit.CalUnitParams.Diffuser


  sealed trait GcalConfig {
    val lampAr: ArLampState
    val lampCuAr: CuArLampState
    val lampQh: QHLampState
    val lampThAr: ThArLampState
    val lampXe: XeLampState
    val lampIrO: Option[IrLampState]
    val shutter: Shutter
    val filterO: Option[Filter]
    val diffuserO: Option[Diffuser]
  }


  object GcalConfig {

    final case class GcalOn(
                             lampAr: ArLampState,
                             lampCuAr: CuArLampState,
                             lampQh: QHLampState,
                             lampThAr: ThArLampState,
                             lampXe: XeLampState,
                             lampIrO: Option[IrLampState],
                             shutter: Shutter,
                             filter: Filter,
                             diffuser: Diffuser
                           ) extends GcalConfig {
      override val filterO: Option[Filter] = filter.some
      override val diffuserO: Option[Diffuser] = diffuser.some
    }

    case object GcalOff extends GcalConfig {
      override val lampAr: ArLampState = ArLampState(LampState.Off)
      override val lampCuAr: CuArLampState = CuArLampState(LampState.Off)
      override val lampQh: QHLampState = QHLampState(LampState.Off)
      override val lampThAr: ThArLampState = ThArLampState(LampState.Off)
      override val lampXe: XeLampState = XeLampState(LampState.Off)
      override val lampIrO: Option[IrLampState] = IrLampState(LampState.Off).some
      override val shutter: Shutter = Shutter.CLOSED
      override val filterO: Option[Filter] = none
      override val diffuserO: Option[Diffuser] = none
    }

    // This configuration is for observations that do not use GCAL. It is preferable to not turn off the IR lamp.
    case object GcalOffIgnoringIr extends GcalConfig {
      override val lampAr: ArLampState = ArLampState(LampState.Off)
      override val lampCuAr: CuArLampState = CuArLampState(LampState.Off)
      override val lampQh: QHLampState = QHLampState(LampState.Off)
      override val lampThAr: ThArLampState = ThArLampState(LampState.Off)
      override val lampXe: XeLampState = XeLampState(LampState.Off)
      override val lampIrO: Option[IrLampState] = none
      override val shutter: Shutter = Shutter.CLOSED
      override val filterO: Option[Filter] = none
      override val diffuserO: Option[Diffuser] = none
    }

  }

  implicit val gcalConfigShow: Show[GcalConfig] = Show.show( config =>
    List(
      s"lampAr = ${config.lampAr}",
      s"lampCuar = ${config.lampCuAr}",
      s"lampQH = ${config.lampQh}",
      s"lampThAr = ${config.lampThAr}",
      s"lampXe = ${config.lampXe}",
      s"lampIr = ${config.lampIrO}",
      s"shutter = ${config.shutter}",
      s"filter = ${config.filterO}",
      s"diffuser = ${config.diffuserO}"
    ).mkString
  )

}
