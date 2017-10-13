// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.gcal

import edu.gemini.seqexec.server.{EpicsCodex, SeqAction}
import edu.gemini.spModel.gemini.calunit.CalUnitParams.{Diffuser, Filter, Shutter}

import scalaz.Scalaz._

/**
  * Created by jluhrs on 3/16/17.
  */
object GcalControllerEpics extends GcalController {
  import EpicsCodex._
  import GcalController._

  implicit private val decodeLampState: DecodeEpicsValue[BinaryOnOff, LampState] = DecodeEpicsValue {
    (v: BinaryOnOff) =>
      v match {
        case BinaryOnOff.OFF => LampState.Off
        case BinaryOnOff.ON  => LampState.On
        case _               => sys.error("Cannot happen")
      }
  }

  implicit private val encodeLampState: EncodeEpicsValue[LampState, BinaryOnOff] = EncodeEpicsValue {
    (v: LampState) =>
      v match {
        case LampState.Off => BinaryOnOff.OFF
        case LampState.On  => BinaryOnOff.ON
        case _             => sys.error("Cannot happen")
      }
  }

  implicit private val decodeShutter: DecodeEpicsValue[String, Option[Shutter]] = DecodeEpicsValue {
    (v: String) =>
      v match {
        case "OPEN"  => Some(Shutter.OPEN)
        case "CLOSE" => Some(Shutter.CLOSED)
        case _       => None
      }
  }

  implicit private val encodeShutter: EncodeEpicsValue[Shutter, String] = EncodeEpicsValue {
    (v: Shutter) =>
      v match {
        case Shutter.OPEN   => "OPEN"
        case Shutter.CLOSED => "CLOSE"
      }
  }

  implicit private val decodeFilter: DecodeEpicsValue[String, Option[Filter]] = DecodeEpicsValue {
    (v: String) =>
      v match {
        case "CLEAR" => Some(Filter.NONE)
        case "GMOS"  => Some(Filter.GMOS)
        case "HROS"  => Some(Filter.HROS)
        case "NIR"   => Some(Filter.NIR)
        case "ND1.0" => Some(Filter.ND_10)
        case "ND2.0" => Some(Filter.ND_20)
        case "ND3.0" => Some(Filter.ND_30)
        case "ND4.0" => Some(Filter.ND_40)
        case "ND4-5" => Some(Filter.ND_45)
        case _       => None
      }
  }

  implicit private val encodeFilter: EncodeEpicsValue[Filter, String] = EncodeEpicsValue {
    (v: Filter) =>
      v match {
        case Filter.NONE => "CLEAR"
        case Filter.GMOS => "GMOS"
        case Filter.HROS => "HROS"
        case Filter.NIR => "NIR"
        case Filter.ND_10 => "ND1.0"
        case Filter.ND_20 => "ND2.0"
        case Filter.ND_30 => "ND3.0"
        case Filter.ND_40 => "ND4.0"
        case Filter.ND_45 => "ND4-5"
        case _ => "CLEAR"
      }
  }

  implicit private val decodeDiffuser: DecodeEpicsValue[String, Option[Diffuser]] = DecodeEpicsValue {
    (v: String) =>
      v match {
        case "IR"      => Some(Diffuser.IR)
        case "VISIBLE" => Some(Diffuser.VISIBLE)
        case _         => None
      }
  }

  implicit private val encodeDiffuser: EncodeEpicsValue[Diffuser, String] = EncodeEpicsValue {
    (v: Diffuser) =>
      v match {
        case Diffuser.IR => "IR"
        case Diffuser.VISIBLE => "VISIBLE"
      }
  }

  private def getDiffuser: Option[Diffuser] = for {
    x <- GcalEpics.instance.diffuser
    y <- decode[String, Option[Diffuser]](x)
  } yield y

  private def getFilter: Option[Filter] = for {
    x <- GcalEpics.instance.filter
    y <- decode[String, Option[Filter]](x)
  } yield y

  private def getShutter: Option[Shutter] = for {
    x <- GcalEpics.instance.filter
    y <- decode[String, Option[Shutter]](x)
  } yield y

  private def getArLamp: Option[ArLampState] = GcalEpics.instance.lampAr().map(decode[BinaryOnOff, LampState]).map(ArLampState.apply)

  private def getCuArLamp: Option[CuArLampState] = GcalEpics.instance.lampCuAr().map(decode[BinaryOnOff, LampState]).map(CuArLampState.apply)

  private def getQHLamp: Option[QHLampState] = GcalEpics.instance.lampQH().map(decode[BinaryOnOff, LampState]).map(QHLampState.apply)

  private def getThArLamp: Option[ThArLampState] = GcalEpics.instance.lampThAr().map(decode[BinaryOnOff, LampState]).map(ThArLampState.apply)

  private def getXeLamp: Option[XeLampState] = GcalEpics.instance.lampXe().map(decode[BinaryOnOff, LampState]).map(XeLampState.apply)

  private def getIrLamp: Option[IrLampState] = GcalEpics.instance.lampIr().map(decode[BinaryOnOff, LampState]).map(IrLampState.apply)

  override def getConfig: SeqAction[GcalConfig] = SeqAction(GcalConfig(getArLamp, getCuArLamp, getQHLamp,
    getThArLamp, getXeLamp, getIrLamp, getShutter, getFilter, getDiffuser)
  )

  override def applyConfig(config: GcalConfig): SeqAction[Unit] = {
    def setArLampParams(v: BinaryOnOff): List[SeqAction[Unit]] = List(
      GcalEpics.instance.lampsCmd.setArLampName("Ar"),
      GcalEpics.instance.lampsCmd.setArLampOn(v)
    )

    def setCuArLampParams(v: BinaryOnOff): List[SeqAction[Unit]] = List(
      GcalEpics.instance.lampsCmd.setCuArLampName("CuAr"),
      GcalEpics.instance.lampsCmd.setCuArLampOn(v)
    )

    def setThArLampParams(v: BinaryOnOff): List[SeqAction[Unit]] = List(
      GcalEpics.instance.lampsCmd.setThArLampName("ThAr"),
      GcalEpics.instance.lampsCmd.setThArLampOn(v)
    )

    def setQHLampParams(v: BinaryOnOff): List[SeqAction[Unit]] = List(
      GcalEpics.instance.lampsCmd.setQHLampName("QH"),
      GcalEpics.instance.lampsCmd.setQHLampOn(v)
    )

    def setXeLampParams(v: BinaryOnOff): List[SeqAction[Unit]] = List(
      GcalEpics.instance.lampsCmd.setXeLampName("Xe"),
      GcalEpics.instance.lampsCmd.setXeLampOn(v)
    )

    def setIrLampParams(v: BinaryOnOff): List[SeqAction[Unit]] = List(
      GcalEpics.instance.lampsCmd.setIRLampName("IR"),
      GcalEpics.instance.lampsCmd.setIRLampOn(v)
    )

    val params: List[SeqAction[Unit]] = List(
      config.lampAr.map(v => setArLampParams(encode(v.self))),
      config.lampCuAr.map(v => setCuArLampParams(encode(v.self))),
      config.lampThAr.map(v => setThArLampParams(encode(v.self))),
      config.lampQh.map(v => setQHLampParams(encode(v.self))),
      config.lampXe.map(v => setXeLampParams(encode(v.self))),
      config.lampIr.map(v => setIrLampParams(encode(v.self)))
    ).toList.collect { case Some(x) => x }.flatten ++
      List(
        config.shutter.map(v => GcalEpics.instance.shutterCmd.setPosition(encode(v))),
        config.filter.map(v => GcalEpics.instance.filterCmd.setName(encode(v))),
        config.diffuser.map(v => GcalEpics.instance.diffuserCmd.setName(encode(v)))
      ).collect { case Some(x) => x }

    if (params.isEmpty) SeqAction(())
    else for {
      _ <- params.sequence
      _ <- GcalEpics.instance.post
    } yield ()
  }
}
