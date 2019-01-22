// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import cats.implicits._
import cats.effect.Sync
import giapi.client.commands.Configuration
import giapi.client.GiapiStatusDb
import giapi.client.syntax.status._

final case class GpiStatusApply(tp:          String,
                                status:      String,
                                configParam: String) {

  def intCompare[F[_]: Sync](statusDb: GiapiStatusDb[F],
                             config:   Configuration): F[Configuration] =
    (statusDb
      .value(status)
      .map(_.intCfg === config.value(configParam)))
      .ifM(Sync[F].delay(config.remove(configParam)), Sync[F].pure(config))

  def doubleCompare[F[_]: Sync](statusDb: GiapiStatusDb[F],
                                config:   Configuration): F[Configuration] =
    (statusDb
      .value(status)
      .map(x =>
        x.doubleCfg === config.value(configParam)
      ))
      .ifM(Sync[F].delay(config.remove(configParam)), Sync[F].pure(config))

  def floatCompare[F[_]: Sync](statusDb: GiapiStatusDb[F],
                               config:   Configuration): F[Configuration] =
    (statusDb
      .value(status)
      .map(x =>
        x.floatCfg === config.value(configParam)
      ))
      .ifM(Sync[F].delay(config.remove(configParam)), Sync[F].pure(config))

  def dfloatCompare[F[_]: Sync](statusDb: GiapiStatusDb[F],
                               config:   Configuration): F[Configuration] =
    (statusDb
      .value(status)
      .map(x =>
        x.floatCfg === config.value(configParam)
      ))
      .ifM(Sync[F].delay(config.remove(configParam)), Sync[F].pure(config))

  def stringCompare[F[_]: Sync](statusDb: GiapiStatusDb[F],
                               config:   Configuration): F[Configuration] =
    (statusDb
      .value(status)
      .map(x =>
        x.stringCfg === config.value(configParam)
      ))
      .ifM(Sync[F].delay(config.remove(configParam)), Sync[F].pure(config))

  def compare[F[_]: Sync](statusDb: GiapiStatusDb[F],
                          config:   Configuration): F[Configuration] = tp match {
    case "INT"    => intCompare(statusDb, config)
    case "DOUBLE" => doubleCompare(statusDb, config)
    case "FLOAT"  => floatCompare(statusDb, config)
    case "DFLOAT"  => dfloatCompare(statusDb, config)
    case "STRING"  => stringCompare(statusDb, config)
    case _        => sys.error("Unknown")
  }
}

object GpiStatusApply {
  val adc = GpiStatusApply("INT", "gpi:adcDeploy", "gpi:selectAdc.deploy")
  // val useAo = GpiStatusApply("INT", "gpi:ao:optimization", "gpi:configAo.useAo")
  val aoOptimize =
    GpiStatusApply("INT", "gpi:ao:optimization", "gpi:configAo.optimize")
  // val useCal = GpiStatusApply("INT", "gpi:ao:optimization", "gpi:configAo.useCal")
  // val fpmPinholeBias = GpiStatusApply("INT", "gpi:ao:optimization", "gpi:configCal.fpmPinholeBias")
  val integrationTime = GpiStatusApply("FLOAT",
                                       "gpi:currentIntegrationTime",
                                       "gpi:configIfs.integrationTime")
  val numCoadds =
    GpiStatusApply("INT", "gpi:currentNumCoadds", "gpi:configIfs.numCoadds")
  val magI =
    GpiStatusApply("FLOAT", "gpi:starIntensity", "gpi:configAo.magnitudeI")
  val magH = GpiStatusApply("FLOAT", "gpi:cal:magH", "gpi:configAo.magnitudeH")
  val calEntranceShutter = GpiStatusApply(
    "INT",
    "gpi:calEntranceShutter",
    "gpi:selectShutter.calEntranceShutter")
  val calReferenceShutter = GpiStatusApply(
    "INT",
    "gpi:referenceShutter",
    "gpi:selectShutter.calReferenceShutter")
  val calScienceShutter = GpiStatusApply("INT",
                                         "gpi:scienceShutter",
                                         "gpi:selectShutter.calScienceShutter")
  val entranceShutter = GpiStatusApply("INT",
                                       "gpi:omssEntranceShutter",
                                       "gpi:selectShutter.entranceShutter")
  val calExitShutter = GpiStatusApply("INT",
                                      "gpi:calExitShutter",
                                      "gpi:selectShutter.calExitShutter")
  val pupilCamera = GpiStatusApply("INT",
                                   "gpi:pupilViewingMirror",
                                   "gpi:selectPupilCamera.deploy")
  val scPower = GpiStatusApply("FLOAT",
                               "gpi:artificialSourceSCpower",
                               "gpi:selectSource.sourceSCpower")
  val scAttenuation = GpiStatusApply("FLOAT",
                                     "gpi:artificialSourceSCDb",
                                     "gpi:selectSource.sourceSCatten")
  val srcVis = GpiStatusApply("INT",
                              "gpi:artificialSourceVIS",
                              "gpi:selectSource.sourceVis")
  val srcIR =
    GpiStatusApply("INT", "gpi:artificialSourceIR", "gpi:selectSource.sourceIr")
  // val polarizerDeploy = GpiStatusApply("INT", "gpi:artificialSourceIR", "gpi:configPolarizer.deploy")
  val polarizerAngle =
    GpiStatusApply("DFLOAT", "gpi:polarizerAngle", "gpi:configPolarizer.angle")

  val observationMode =
    GpiStatusApply("STRING", "gpi:observationMode", "gpi:observationMode.mode")

  val ifsFilter =
    GpiStatusApply("STRING", "gpi:ifsFilter", "gpi:ifs:selectIfsFilter.maskStr")

  val ppm =
    GpiStatusApply("STRING", "gpi:ppmMask", "gpi:selectPupilPlaneMask.maskStr")

  val fpm =
    GpiStatusApply("STRING", "gpi:fpmMask", "gpi:selectFocalPlaneMask.maskStr")

  val lyot =
    GpiStatusApply("STRING", "gpi:lyotMask", "gpi:selectLyotMask.maskStr")

  val all = List(
    adc,
    aoOptimize,
    integrationTime,
    numCoadds,
    magI,
    magH,
    calEntranceShutter,
    calReferenceShutter,
    calScienceShutter,
    entranceShutter,
    calExitShutter,
    pupilCamera,
    scPower,
    scAttenuation,
    srcVis,
    srcIR,
    polarizerAngle,
    observationMode,
    ifsFilter,
    ppm,
    fpm,
    lyot
  )

  def foldConfigM[F[_]: Sync](db:     GiapiStatusDb[F],
                              config: Configuration): F[Configuration] =
    all.foldLeftM(config) { (c, i) =>
      i.compare(db, c)
    }

  val statusesToMonitor = all.map(_.status)
}
