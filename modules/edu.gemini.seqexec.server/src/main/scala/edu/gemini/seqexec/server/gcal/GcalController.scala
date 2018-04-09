// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.gcal

import edu.gemini.seqexec.server.SeqAction
import edu.gemini.spModel.gemini.calunit.CalUnitParams.Shutter

import cats.Eq
import cats.implicits._

trait GcalController {

  import GcalController._

  def getConfig: SeqAction[GcalConfig]

  def applyConfig(config: GcalConfig): SeqAction[Unit]

}

object GcalController {

  trait LampState

  object LampState {

    case object Off extends LampState

    case object On extends LampState

    implicit val eq: Eq[LampState] =
      Eq.fromUniversalEquals

  }

  final case class ArLampState(self: LampState) extends AnyVal

  object ArLampState {
    implicit val eq: Eq[ArLampState] =
      Eq[LampState].contramap(_.self)
  }

  final case class CuArLampState(self: LampState) extends AnyVal

  object CuArLampState {
    implicit val eq: Eq[CuArLampState] =
      Eq[LampState].contramap(_.self)
  }

  final case class QHLampState(self: LampState) extends AnyVal

  object QHLampState {
    implicit val eq: Eq[QHLampState] =
      Eq[LampState].contramap(_.self)
  }

  final case class ThArLampState(self: LampState) extends AnyVal

  object ThArLampState {
    implicit val eq: Eq[ThArLampState] =
      Eq[LampState].contramap(_.self)
  }

  final case class XeLampState(self: LampState) extends AnyVal

  object XeLampState {
    implicit val eq: Eq[XeLampState] =
      Eq[LampState].contramap(_.self)
  }

  final case class IrLampState(self: LampState) extends AnyVal

  object IrLampState {
    implicit val eq: Eq[IrLampState] =
      Eq[LampState].contramap(_.self)
  }

  type Shutter = edu.gemini.spModel.gemini.calunit.CalUnitParams.Shutter

  type Filter = edu.gemini.spModel.gemini.calunit.CalUnitParams.Filter

  type Diffuser = edu.gemini.spModel.gemini.calunit.CalUnitParams.Diffuser

  final case class GcalConfig(
                         lampAr: Option[ArLampState],
                         lampCuAr: Option[CuArLampState],
                         lampQh: Option[QHLampState],
                         lampThAr: Option[ThArLampState],
                         lampXe: Option[XeLampState],
                         lampIr: Option[IrLampState],
                         shutter: Option[Shutter],
                         filter: Option[Filter],
                         diffuser: Option[Diffuser]
                       )

  object GcalConfig {

    val allOff: GcalConfig = GcalConfig(Some(ArLampState(LampState.Off)),
      Some(CuArLampState(LampState.Off)),
      Some(QHLampState(LampState.Off)),
      Some(ThArLampState(LampState.Off)),
      Some(XeLampState(LampState.Off)),
      Some(IrLampState(LampState.Off)),
      Some(Shutter.CLOSED),
      None,
      None
    )

    def fullConfig(ar: ArLampState,
                   cuAr: CuArLampState,
                   qh: QHLampState,
                   thAr: ThArLampState,
                   xe: XeLampState,
                   ir: IrLampState,
                   sh: Shutter,
                   flt: Filter,
                   diff: Diffuser
                  ): GcalConfig = GcalConfig(Some(ar), Some(cuAr), Some(qh), Some(thAr), Some(xe),
      Some(ir), Some(sh), Some(flt), Some(diff))

  }

}
