// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import cats.Monad
import cats.effect.IO
import cats.effect.Sync
import cats.data.OptionT
import cats.implicits._
import squants.Time
import edu.gemini.spModel.gemini.calunit.CalUnitParams.{Diffuser, Filter, Shutter}
import edu.gemini.seqexec.server.gcal.BinaryOnOff
import seqexec.server.EpicsCodex._
import seqexec.server.gcal.GcalController._
import squants.time.Seconds

object GcalControllerEpics {
  // Default value from Tcl Seqexec
  private val SetupTimeout: Time = Seconds(60)

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

  private def getDiffuser[F[_]: Monad](sys: GcalEpics[F]): F[Option[Diffuser]] =
    (for {
      x <- OptionT(sys.diffuser)
      y <- OptionT.fromOption[F](decode[String, Option[Diffuser]](x))
    } yield y).value

  private def getFilter[F[_]: Monad](sys: GcalEpics[F]): F[Option[Filter]] =
    (for {
      x <- OptionT(sys.filter)
      y <- OptionT.fromOption[F](decode[String, Option[Filter]](x))
    } yield y).value

  private def getShutter[F[_]: Monad](sys: GcalEpics[F]): F[Option[Shutter]] =
    (for {
      x <- OptionT(sys.filter)
      y <- OptionT.fromOption[F](decode[String, Option[Shutter]](x))
    } yield y).value

  private def getArLamp[F[_]: Sync](sys: GcalEpics[F]): F[Option[ArLampState]] =
    (for {
      x <- OptionT(sys.lampAr.delay[F])
      y <- OptionT.some[F](decode[BinaryOnOff, LampState](x))
    } yield ArLampState(y)).value

  private def getCuArLamp[F[_]: Sync](sys: GcalEpics[F]): F[Option[CuArLampState]] =
    (for {
      x <- OptionT(sys.lampCuAr.delay[F])
      y <- OptionT.some[F](decode[BinaryOnOff, LampState](x))
    } yield CuArLampState(y)).value

  private def getQHLamp[F[_]: Sync](sys: GcalEpics[F]): F[Option[QHLampState]] =
    (for {
      x <- OptionT(sys.lampQH.delay[F])
      y <- OptionT.some[F](decode[BinaryOnOff, LampState](x))
    } yield QHLampState(y)).value

  private def getThArLamp[F[_]: Sync](sys: GcalEpics[F]): F[Option[ThArLampState]] =
    (for {
      x <- OptionT(sys.lampThAr.delay[F])
      y <- OptionT.some[F](decode[BinaryOnOff, LampState](x))
    } yield ThArLampState(y)).value

  private def getXeLamp[F[_]: Sync](sys: GcalEpics[F]): F[Option[XeLampState]] =
    (for {
      x <- OptionT(sys.lampXe.delay[F])
      y <- OptionT.some[F](decode[BinaryOnOff, LampState](x))
    } yield XeLampState(y)).value

  private def getIrLamp[F[_]: Sync](sys: GcalEpics[F]): F[Option[IrLampState]] =
    (for {
      x <- OptionT(sys.lampIr.delay[F])
      y <- OptionT.some[F](decode[BinaryOnOff, LampState](x))
    } yield IrLampState(y)).value

  private def setArLampParams(v: BinaryOnOff): List[IO[Unit]] = List(
    GcalEpics.instance.lampsCmd.setArLampName("Ar"),
    GcalEpics.instance.lampsCmd.setArLampOn(v)
  )

  private def setCuArLampParams(v: BinaryOnOff): List[IO[Unit]] = List(
    GcalEpics.instance.lampsCmd.setCuArLampName("CuAr"),
    GcalEpics.instance.lampsCmd.setCuArLampOn(v)
  )

  private def setThArLampParams(v: BinaryOnOff): List[IO[Unit]] = List(
    GcalEpics.instance.lampsCmd.setThArLampName("ThAr"),
    GcalEpics.instance.lampsCmd.setThArLampOn(v)
  )

  private def setQHLampParams(v: BinaryOnOff): List[IO[Unit]] = List(
    GcalEpics.instance.lampsCmd.setQHLampName("QH"),
    GcalEpics.instance.lampsCmd.setQHLampOn(v)
  )

  private def setXeLampParams(v: BinaryOnOff): List[IO[Unit]] = List(
    GcalEpics.instance.lampsCmd.setXeLampName("Xe"),
    GcalEpics.instance.lampsCmd.setXeLampOn(v)
  )

  private def setIrLampParams(v: BinaryOnOff): List[IO[Unit]] = List(
    GcalEpics.instance.lampsCmd.setIRLampName("IR"),
    GcalEpics.instance.lampsCmd.setIRLampOn(v)
  )

  def apply(): GcalController[IO] = new GcalController[IO] {
    override def getConfig: IO[GcalConfig] =
      (getArLamp(GcalEpics.instance),
       getCuArLamp(GcalEpics.instance),
       getQHLamp(GcalEpics.instance),
       getThArLamp(GcalEpics.instance),
       getXeLamp(GcalEpics.instance),
       getIrLamp(GcalEpics.instance),
       getShutter(GcalEpics.instance),
       getFilter(GcalEpics.instance),
       getDiffuser(GcalEpics.instance)).mapN(GcalConfig.apply)

    override def applyConfig(config: GcalConfig): IO[Unit] = {
      val params: List[IO[Unit]] = List(
        config.lampAr.map(v => setArLampParams(encode(v.self))),
        config.lampCuAr.map(v => setCuArLampParams(encode(v.self))),
        config.lampThAr.map(v => setThArLampParams(encode(v.self))),
        config.lampQh.map(v => setQHLampParams(encode(v.self))),
        config.lampXe.map(v => setXeLampParams(encode(v.self))),
        config.lampIr.map(v => setIrLampParams(encode(v.self)))
      ).collect { case Some(x) => x }.flatten ++
        List(
          config.shutter.map(v => GcalEpics.instance.shutterCmd.setPosition(encode(v))),
          config.filter.map(v => GcalEpics.instance.filterCmd.setName(encode(v))),
          config.diffuser.map(v => GcalEpics.instance.diffuserCmd.setName(encode(v)))
        ).collect { case Some(x) => x }

      (params.sequence *>
        GcalEpics.instance.lampsCmd.setTimeout[IO](SetupTimeout) *>
        GcalEpics.instance.post).whenA(params.nonEmpty)
    }
  }
}
