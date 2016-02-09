package edu.gemini.seqexec.server

import java.util.logging.Logger

import edu.gemini.seqexec.server.DhsClient.ObsId
import edu.gemini.seqexec.server.Flamingos2Controller._
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.Decker
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.Filter
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.ReadoutMode
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.WindowCover
import edu.gemini.spModel.gemini.flamingos2.Flamingos2._

import scalaz.Scalaz._

import scalaz.EitherT
import scalaz.concurrent.Task

/**
 * Created by jluhrs on 11/16/15.
 */
object Flamingos2ControllerEpics extends Flamingos2Controller {
  private val Log = Logger.getLogger(getClass.getName)

  import EpicsCodex._

  override def getConfig: SeqAction[Flamingos2Config] = ???

  implicit val encodeReadoutMode: EncodeEpicsValue[ReadoutMode, String] = EncodeEpicsValue((a: ReadoutMode)
    => a match {
      case ReadoutMode.SCIENCE     => "SCI"
      case ReadoutMode.ENGINEERING => "ENG"
    }
  )

  implicit val encodeBiasMode: EncodeEpicsValue[BiasMode, String] = EncodeEpicsValue((a: BiasMode)
    => a match {
      case BiasMode.Imaging  => "Imaging"
      case BiasMode.LongSlit => "Long_Slit"
      case BiasMode.MOS      => "Mos"
    }
  )

  def setDCConfig(dc: DCConfig): SeqAction[Unit] = for {
    _ <- Flamingos2Epics.instance.dcConfigCmd.setExposureTime(dc.t.toSeconds)
    _ <- Flamingos2Epics.instance.dcConfigCmd.setNumReads(dc.n.getCount)
    _ <- Flamingos2Epics.instance.dcConfigCmd.setReadoutMode(encode(dc.r))
    _ <- Flamingos2Epics.instance.dcConfigCmd.setBiasMode(encode(dc.b))
  } yield ()

  implicit val encodeWindowCoverPosition: EncodeEpicsValue[WindowCover, String] = EncodeEpicsValue((a: WindowCover)
    => a match {
      case WindowCover.OPEN  => "Open"
      case WindowCover.CLOSE => "Closed"
    }
  )

  implicit val encodeDeckerPosition: EncodeEpicsValue[Decker, String] = EncodeEpicsValue((a: Decker)
    => a match {
      case Decker.IMAGING   => "Open"
      case Decker.LONG_SLIT => "Long_Slit"
      case Decker.MOS       => "Mos"
    }
  )

  implicit val encodeFPUPosition: EncodeEpicsValue[FocalPlaneUnit, (String, String)] = EncodeEpicsValue((a: FocalPlaneUnit)
    => a match {
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
  )

  implicit val encodeFilterPosition: EncodeEpicsValue[Filter, String] = EncodeEpicsValue((a: Filter)
    => a match {
      case Filter.OPEN    => "Open"
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
      case Filter.DARK    => "DK_G0807"
    }
  )

  implicit val encodeLyotPosition: EncodeEpicsValue[Lyot, String] = EncodeEpicsValue((a: Lyot)
    => a match {
      case LyotWheel.OPEN       => "f/16_G5830"
      case LyotWheel.HIGH       => "null"
      case LyotWheel.LOW        => "null"
      case LyotWheel.GEMS_OVER  => "GEMS_over_G5836"
      case LyotWheel.GEMS_UNDER => "GEMS_under_G5835"
      case LyotWheel.GEMS       => "Gems_G5835"
      case LyotWheel.H1         => "Hart1_G5833"
      case LyotWheel.H2         => "Hart2_G5834"
    }
  )

  implicit val encodeGrismPosition: EncodeEpicsValue[Grism, String] = EncodeEpicsValue((a: Grism)
    => a match {
      case Disperser.NONE  => "Open"
      case Disperser.R1200HK => "HK_G5802"
      case Disperser.R1200JH => "JH_G5801"
      case Disperser.R3000 => "R3K_G5803"
    }
  )

  def setCCConfig(cc: CCConfig): SeqAction[Unit] = {
    val fpu = encode(cc.fpu)
    for {
      _ <- Flamingos2Epics.instance.configCmd.setWindowCover(encode(cc.w))
      _ <- Flamingos2Epics.instance.configCmd.setDecker(encode(cc.d))
      _ <- Flamingos2Epics.instance.configCmd.setMOS(fpu._1)
      _ <- Flamingos2Epics.instance.configCmd.setMask(fpu._2)
      _ <- Flamingos2Epics.instance.configCmd.setFilter(encode(cc.f))
      _ <- Flamingos2Epics.instance.configCmd.setLyot(encode(cc.l))
      _ <- Flamingos2Epics.instance.configCmd.setGrism(encode(cc.g))
    } yield ()
  }

  override def applyConfig(config: Flamingos2Config): SeqAction[Unit] = for {
    _ <- EitherT(Task(Log.info("Start Flamingos2 configuration").right))
    _ <- setDCConfig(config.dc)
    _ <- setCCConfig(config.cc)
    _ <- Flamingos2Epics.instance.post
    _ <- EitherT(Task(Log.info("Completed Flamingos2 configuration").right))
  } yield ()

  override def observe(obsid: ObsId): SeqAction[ObsId] = for {
    _ <- EitherT(Task(Log.info("Start Flamingos2 observation").right))
    _ <- Flamingos2Epics.instance.observeCmd.setLabel(obsid)
    _ <- Flamingos2Epics.instance.observeCmd.post
    _ <- EitherT(Task(Log.info("Completed Flamingos2 observation").right))
  } yield obsid
}
