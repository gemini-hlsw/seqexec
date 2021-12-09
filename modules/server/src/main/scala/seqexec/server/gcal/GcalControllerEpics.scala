// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import java.util.concurrent.TimeUnit.SECONDS

import scala.concurrent.duration.FiniteDuration

import cats.effect.Async
import cats.syntax.all._
import edu.gemini.seqexec.server.gcal.BinaryOnOff
import edu.gemini.spModel.gemini.calunit.CalUnitParams.Diffuser
import edu.gemini.spModel.gemini.calunit.CalUnitParams.Filter
import edu.gemini.spModel.gemini.calunit.CalUnitParams.Shutter
import org.typelevel.log4cats.Logger
import seqexec.server.EpicsCodex._
import seqexec.server.EpicsUtil.applyParam
import seqexec.server.gcal.GcalController.Diffuser
import seqexec.server.gcal.GcalController.Filter
import seqexec.server.gcal.GcalController.Shutter
import seqexec.server.gcal.GcalController._

object GcalControllerEpics {
  // Default value from Tcl Seqexec
  private val SetupTimeout: FiniteDuration = FiniteDuration(60, SECONDS)

  implicit private val encodeLampState: EncodeEpicsValue[LampState, BinaryOnOff] =
    EncodeEpicsValue {
      case LampState.Off => BinaryOnOff.OFF
      case LampState.On  => BinaryOnOff.ON
      case _             => sys.error("Cannot happen")
    }

  implicit private val encodeShutter: EncodeEpicsValue[Shutter, String] = EncodeEpicsValue {
    case Shutter.OPEN   => "OPEN"
    case Shutter.CLOSED => "CLOSE"
  }

  implicit private val encodeFilter: EncodeEpicsValue[Filter, String] = EncodeEpicsValue {
    case Filter.NONE  => "CLEAR"
    case Filter.GMOS  => "GMOS"
    case Filter.HROS  => "HROS"
    case Filter.NIR   => "NIR"
    case Filter.ND_10 => "ND1.0"
    case Filter.ND_20 => "ND2.0"
    case Filter.ND_30 => "ND3.0"
    case Filter.ND_40 => "ND4.0"
    case Filter.ND_45 => "ND4-5"
    case _            => "CLEAR"
  }

  implicit private val encodeDiffuser: EncodeEpicsValue[Diffuser, String] = EncodeEpicsValue {
    case Diffuser.IR      => "IR"
    case Diffuser.VISIBLE => "VISIBLE"
  }

  private def setArLampParams[F[_]: Async](sys: GcalEpics[F])(v: BinaryOnOff): F[Unit] =
    sys.lampsCmd.setArLampName("Ar") *>
      sys.lampsCmd.setArLampOn(v)

  private def setCuArLampParams[F[_]: Async](sys: GcalEpics[F])(v: BinaryOnOff): F[Unit] =
    sys.lampsCmd.setCuArLampName("CuAr") *>
      sys.lampsCmd.setCuArLampOn(v)

  private def setThArLampParams[F[_]: Async](sys: GcalEpics[F])(v: BinaryOnOff): F[Unit] =
    sys.lampsCmd.setThArLampName("ThAr") *>
      sys.lampsCmd.setThArLampOn(v)

  private def setQH5WLampParams[F[_]: Async](sys: GcalEpics[F])(v: BinaryOnOff): F[Unit] =
    sys.lampsCmd.setQH5WLampName("QH") *>
      sys.lampsCmd.setQH5WLampOn(v)

  private def setQH100WLampParams[F[_]: Async](sys: GcalEpics[F])(v: BinaryOnOff): F[Unit] =
    sys.lampsCmd.setQH100WLampName("QH100") *>
      sys.lampsCmd.setQH100WLampOn(v)

  private def setXeLampParams[F[_]: Async](sys: GcalEpics[F])(v: BinaryOnOff): F[Unit] =
    sys.lampsCmd.setXeLampName("Xe") *>
      sys.lampsCmd.setXeLampOn(v)

  private def setIrLampParams[F[_]: Async](sys: GcalEpics[F])(v: BinaryOnOff): F[Unit] =
    sys.lampsCmd.setIRLampName("IR") *>
      sys.lampsCmd.setIRLampOn(v)

  def apply[F[_]: Async: Logger](epics: => GcalEpics[F]): GcalController[F] =
    new GcalController[F] {
      override def applyConfig(config: GcalConfig): F[Unit] =
        retrieveConfig(epics).flatMap(configure(epics, _, config))
    }

  final case class EpicsGcalConfig(
    lampAr:     BinaryOnOff,
    lampCuAr:   BinaryOnOff,
    lampQh5W:   BinaryOnOff,
    lampQh100W: BinaryOnOff,
    lampThAr:   BinaryOnOff,
    lampXe:     BinaryOnOff,
    lampIr:     BinaryOnOff,
    shutter:    String,
    filter:     String,
    diffuser:   String
  )

  def retrieveConfig[F[_]: Async](epics: GcalEpics[F]): F[EpicsGcalConfig] = for {
    ar    <- epics.lampAr
    cuAr  <- epics.lampCuAr
    qh5   <- epics.lampQH5W
    qh100 <- epics.lampQH100W
    thAr  <- epics.lampThAr
    xe    <- epics.lampXe
    ir    <- epics.lampIr
    shut  <- epics.shutter
    filt  <- epics.filter
    diff  <- epics.diffuser
  } yield EpicsGcalConfig(
    ar,
    cuAr,
    qh5,
    qh100,
    thAr,
    xe,
    ir,
    shut,
    filt,
    diff
  )

  def configure[F[_]: Async](epics: GcalEpics[F], current: EpicsGcalConfig, demand: GcalConfig)(
    implicit L:                     Logger[F]
  ): F[Unit] = {
    val params: List[F[Unit]] = List(
      applyParam(current.lampAr, encode(demand.lampAr.self), setArLampParams(epics)),
      applyParam(current.lampCuAr, encode(demand.lampCuAr.self), setCuArLampParams(epics)),
      applyParam(current.lampQh5W, encode(demand.lampQh5W.self), setQH5WLampParams(epics)),
      applyParam(current.lampQh5W, encode(demand.lampQh100W.self), setQH100WLampParams(epics)),
      applyParam(current.lampThAr, encode(demand.lampThAr.self), setThArLampParams(epics)),
      applyParam(current.lampXe, encode(demand.lampXe.self), setXeLampParams(epics)),
      demand.lampIrO.flatMap(d =>
        applyParam(current.lampIr, encode(d.self), setIrLampParams(epics))
      ),
      applyParam(current.shutter, encode(demand.shutter), epics.shutterCmd.setPosition),
      demand.filterO.flatMap(d => applyParam(current.filter, encode(d), epics.filterCmd.setName)),
      demand.diffuserO.flatMap(d =>
        applyParam(current.diffuser, encode(d), epics.diffuserCmd.setName)
      )
    ).flattenOption

    (for {
      _ <- L.info("Start GCAL configuration")
      _ <- L.debug(s"GCAL configuration: ${demand.show}")
      _ <- params.sequence
      r <- epics.post(SetupTimeout)
      _ <- L.debug("Completed GCAL configuration")
    } yield r).whenA(params.nonEmpty)

  }

}
