package seqexec.server.niri

import cats.Show
import cats.effect.IO
import seqexec.model.dhs.ImageFileId
import seqexec.server.niri.NiriController.NiriConfig
import seqexec.server.{ObserveCommand, Progress, SeqAction}
import squants.{Length, Time}

trait NiriController {

  def applyConfig(config: NiriConfig): SeqAction[Unit]

  def observe(fileId: ImageFileId, expTime: Time): SeqAction[ObserveCommand.Result]

  def endObserve: SeqAction[Unit]

  def stopObserve: SeqAction[Unit]

  def abortObserve: SeqAction[Unit]

  def observeProgress(total: Time): fs2.Stream[IO, Progress]

}

object NiriController {
  type ExposureTime = Time
  type Coadds = Int
  type Camera = edu.gemini.spModel.gemini.niri.Niri.Camera
  type BeamSplitter = edu.gemini.spModel.gemini.niri.Niri.BeamSplitter
  type BuiltInROI = edu.gemini.spModel.gemini.niri.Niri.BuiltinROI
  type Filter = edu.gemini.spModel.gemini.niri.Niri.Filter
  type Focus = edu.gemini.spModel.gemini.niri.Niri.Focus
  type Disperser = edu.gemini.spModel.gemini.niri.Niri.Disperser
  type Mask = edu.gemini.spModel.gemini.niri.Niri.Mask

  sealed trait WindowCover
  object WindowCover {
    case object Closed extends WindowCover
    case object Open extends  WindowCover
  }

  sealed trait ReadMode
  object ReadMode {
    case object LowRN extends ReadMode
    case object MediumRN extends ReadMode
    case object HighRN extends ReadMode
  }

  final case class DCConfig(exposureTime: ExposureTime,
                            coadds: Coadds,
                            readMode: ReadMode,
                            builtInROI: BuiltInROI
                           )
  final case class CCConfig(windowCover: WindowCover,
                            camera: Camera,
                            beamSplitter: BeamSplitter,
                            filter: Filter,
                            focus: Focus,
                            disperser: Disperser,
                            mask: Mask
                           )
  final case class NiriConfig(cc: CCConfig, dc: DCConfig)

  implicit val cfgShow: Show[NiriConfig] = Show.fromToString

}