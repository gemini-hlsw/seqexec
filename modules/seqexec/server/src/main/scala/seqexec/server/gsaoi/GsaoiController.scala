// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gsaoi

import cats.Applicative
import cats.Eq
import cats.Show
import cats.implicits._
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.gsaoi.GsaoiController.DCConfig
import seqexec.server.gsaoi.GsaoiController.GsaoiConfig
import seqexec.server.Progress
import squants.Time
import squants.time.TimeConversions._
import shapeless.tag.@@

trait GsaoiController[F[_]] {

  def applyConfig(config: GsaoiConfig): F[Unit]

  def observe(fileId: ImageFileId, cfg: DCConfig): F[ObserveCommandResult]

  def endObserve: F[Unit]

  def stopObserve: F[Unit]

  def abortObserve: F[Unit]

  def observeProgress(total: Time): fs2.Stream[F, Progress]

  def calcTotalExposureTime(cfg: DCConfig)(
    implicit ev:                 Applicative[F]): F[Time] = {
    val readFactor  = 1.2
    val readOutTime = 15

    (cfg.coadds * cfg.exposureTime * readFactor + readOutTime.seconds).pure[F]
  }

}

trait CoaddsI
trait NumberOfFowSamplesI

sealed trait WindowCover extends Product with Serializable

object WindowCover {
  case object Closed extends WindowCover
  case object Opened extends WindowCover

  implicit val eqWc: Eq[WindowCover] = Eq.fromUniversalEquals
}

object GsaoiController {
  // DC
  type ReadMode           = edu.gemini.spModel.gemini.gsaoi.Gsaoi.ReadMode
  type Roi                = edu.gemini.spModel.gemini.gsaoi.Gsaoi.Roi
  type Coadds             = Int @@ CoaddsI
  type ExposureTime       = Time
  type NumberOfFowSamples = Int @@ NumberOfFowSamplesI

  // CC
  type Filter       = edu.gemini.spModel.gemini.gsaoi.Gsaoi.Filter
  type OdgwSize     = edu.gemini.spModel.gemini.gsaoi.Gsaoi.OdgwSize
  type UtilityWheel = edu.gemini.spModel.gemini.gsaoi.Gsaoi.UtilityWheel

  final case class DCConfig(readMode:           ReadMode,
                            roi:                Roi,
                            coadds:             Coadds,
                            exposureTime:       ExposureTime,
                            numberOfFowSamples: NumberOfFowSamples)

  object DCConfig {
    // Universal equals is fine as it is integers and java classes
    implicit val eqDcConig: Eq[DCConfig] = Eq.fromUniversalEquals
  }

  final case class CCConfig(filter:       Filter,
                            odgwSize:     OdgwSize,
                            utilityWheel: UtilityWheel,
                            windowCover:  WindowCover)

  object CCConfig {
    // Universal equals is fine as it is integers and java classes
    implicit val eqDcConig: Eq[CCConfig] = Eq.fromUniversalEquals
  }

  final case class GsaoiConfig(cc: CCConfig, dc: DCConfig)

  implicit val cfgShow: Show[GsaoiConfig] = Show.fromToString

}
