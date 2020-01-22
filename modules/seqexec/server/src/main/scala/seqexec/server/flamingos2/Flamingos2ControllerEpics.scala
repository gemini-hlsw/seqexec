// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.flamingos2

import cats.data.StateT
import cats.effect.{Async, Timer}
import cats.implicits._
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.{Decker, Filter, ReadoutMode, WindowCover, _}
import io.chrisdavenport.log4cats.Logger
import seqexec.model.ObserveStage
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.{ObsProgress, Progress, ProgressUtil, RemainingTime}
import seqexec.server.flamingos2.Flamingos2Controller._
import seqexec.server.EpicsCodex._
import squants.Time
import squants.time.TimeConversions._

import java.util.concurrent.TimeUnit.{SECONDS, MILLISECONDS}

import scala.concurrent.duration.FiniteDuration

trait Flamingos2Encoders {
  implicit val encodeReadoutMode: EncodeEpicsValue[ReadoutMode, String] = EncodeEpicsValue {
    case ReadoutMode.SCIENCE     => "SCI"
    case ReadoutMode.ENGINEERING => "ENG"
  }

  implicit val encodeBiasMode: EncodeEpicsValue[BiasMode, String] = EncodeEpicsValue {
    case BiasMode.Imaging  => "Imaging"
    case BiasMode.LongSlit => "Long_Slit"
    case BiasMode.MOS      => "Mos"
  }

  implicit val encodeWindowCoverPosition: EncodeEpicsValue[WindowCover, String] = EncodeEpicsValue {
    case WindowCover.OPEN  => "Open"
    case WindowCover.CLOSE => "Closed"
  }

  implicit val encodeDeckerPosition: EncodeEpicsValue[Decker, String] = EncodeEpicsValue {
    case Decker.IMAGING   => "Open"
    case Decker.LONG_SLIT => "Long_Slit"
    case Decker.MOS       => "Mos"
  }

  implicit val encodeFPUPosition: EncodeEpicsValue[FocalPlaneUnit, (String, String)] = EncodeEpicsValue {
    case FocalPlaneUnit.Open        => ("Open", "null")
    case FocalPlaneUnit.GridSub1Pix => ("sub1-pix_grid", "null")
    case FocalPlaneUnit.Grid2Pix    => ("2-pix_grid", "null")
    case FocalPlaneUnit.Slit1Pix    => ("1pix-slit", "null")
    case FocalPlaneUnit.Slit2Pix    => ("2pix-slit", "null")
    case FocalPlaneUnit.Slit3Pix    => ("3pix-slit", "null")
    case FocalPlaneUnit.Slit4Pix    => ("4pix-slit", "null")
    case FocalPlaneUnit.Slit6Pix    => ("6pix-slit", "null")
    case FocalPlaneUnit.Slit8Pix    => ("8pix-slit", "null")
    case FocalPlaneUnit.Custom(s)   => ("null", s)
  }

  // Removed obsolete filter positions Open and DK_G0807
  implicit val encodeFilterPosition: EncodeEpicsValue[Filter, Option[String]] =
    EncodeEpicsValue.applyO {
    case Filter.Y       => "Y_G0811"
    case Filter.F1056   => "F1056"
    case Filter.F1063   => "F1063"
    case Filter.J_LOW   => "J-lo_G0801"
    case Filter.J       => "J_G0802"
    case Filter.H       => "H_G0803"
    case Filter.K_LONG  => "K-long_G0812"
    case Filter.K_SHORT => "Ks_G0804"
    case Filter.JH      => "JH_G0809"
    case Filter.HK      => "HK_G0806"
    case Filter.K_BLUE  => "K-blue_G0814"
    case Filter.K_RED   => "K-red_G0815"
  }

  implicit val encodeLyotPosition: EncodeEpicsValue[Lyot, String] = EncodeEpicsValue {
    case LyotWheel.OPEN       => "f/16_G5830"
    case LyotWheel.HIGH       => "null"
    case LyotWheel.LOW        => "null"
    case LyotWheel.GEMS_OVER  => "GEMS_over_G5836"
    case LyotWheel.GEMS_UNDER => "GEMS_under_G5835"
    case LyotWheel.GEMS       => "Gems_G5835"
    case LyotWheel.H1         => "Hart1_G5833"
    case LyotWheel.H2         => "Hart2_G5834"
  }

  implicit val encodeGrismPosition: EncodeEpicsValue[Grism, String] = EncodeEpicsValue {
    case Grism.Open    => "Open"
    case Grism.R1200HK => "HK_G5802"
    case Grism.R1200JH => "JH_G5801"
    case Grism.R3000   => "R3K_G5803"
    case Grism.Dark    => "DK_G5804"
  }

}

object Flamingos2ControllerEpics extends Flamingos2Encoders {

  val ReadoutTimeout: FiniteDuration = FiniteDuration(30, SECONDS)
  val DefaultTimeout: FiniteDuration = FiniteDuration(60, SECONDS)
  val ConfigTimeout: FiniteDuration = FiniteDuration(400, SECONDS)

  def apply[F[_]: Async](sys: => Flamingos2Epics[F])(implicit tio: Timer[F], L: Logger[F]): Flamingos2Controller[F] = new Flamingos2Controller[F] {

    private def setDCConfig(dc: DCConfig): F[Unit] = for {
      _ <- sys.dcConfigCmd.setExposureTime(dc.t.toSeconds.toDouble)
      _ <- sys.dcConfigCmd.setNumReads(dc.n.getCount)
      _ <- sys.dcConfigCmd.setReadoutMode(encode(dc.r))
      _ <- sys.dcConfigCmd.setBiasMode(encode(dc.b))
    } yield ()

    private def setCCConfig(cc: CCConfig): F[Unit] = {
      val fpu = encode(cc.fpu)
      val filter = encode(cc.f)
      for {
        _ <- sys.configCmd.setWindowCover(encode(cc.w))
        _ <- sys.configCmd.setDecker(encode(cc.d))
        _ <- sys.configCmd.setMOS(fpu._1)
        _ <- sys.configCmd.setMask(fpu._2)
        _ <- filter.map(sys.configCmd.setFilter).getOrElse(Async[F].unit)
        _ <- sys.configCmd.setLyot(encode(cc.l))
        _ <- sys.configCmd.setGrism(encode(cc.g))
      } yield ()
    }

    override def applyConfig(config: Flamingos2Config): F[Unit] = for {
      _ <- L.debug("Start Flamingos2 configuration")
      _ <- setDCConfig(config.dc)
      _ <- setCCConfig(config.cc)
      _ <- sys.post(ConfigTimeout)
      _ <- L.debug("Completed Flamingos2 configuration")
    } yield ()

    override def observe(fileId: ImageFileId, expTime: Time): F[ObserveCommandResult] = for {
      _ <- L.debug(s"Send observe to Flamingos2, file id $fileId")
      _ <- sys.observeCmd.setLabel(fileId)
      _ <- sys.observeCmd.post(FiniteDuration(expTime.toMillis, MILLISECONDS) + ReadoutTimeout)
      _ <- L.debug("Completed Flamingos2 observe")
    } yield ObserveCommandResult.Success

    override def endObserve: F[Unit] = for {
      _ <- L.debug("Send endObserve to Flamingos2")
      _ <- sys.endObserveCmd.mark
      _ <- sys.endObserveCmd.post(DefaultTimeout)
    } yield ()

    override def observeProgress(total: Time): fs2.Stream[F, Progress] = {
      val s = ProgressUtil.fromStateTOption[F, Time](_ => StateT[F, Time, Option[Progress]] { st =>
        val m = if (total >= st) total else st
        val p = for {
          obst <- sys.observeState
          rem <- sys.countdown
          v <- (sys.dcIsPreparing, sys.dcIsAcquiring, sys.dcIsReadingOut).mapN(ObserveStage.fromBooleans)
        } yield if(obst.isBusy) ObsProgress(m, RemainingTime(rem.seconds), v).some else none
        p.map(p => (m, p))
      })
      s(total).dropWhile(_.remaining.self.value === 0.0) // drop leading zeros
        .takeThrough(_.remaining.self.value > 0.0) // drop all tailing zeros but the first one
    }
  }
}
