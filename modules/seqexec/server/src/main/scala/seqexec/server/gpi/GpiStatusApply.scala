// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import cats.implicits._
import cats.effect.Sync
import gem.ocs2.Parsers
import giapi.client.commands.Configuration
import giapi.client.GiapiStatusDb
import giapi.client.syntax.status._
import seqexec.server.gpi.GpiController.GpiConfig

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
      .map(x => x.doubleCfg === config.value(configParam)))
      .ifM(Sync[F].delay(config.remove(configParam)), Sync[F].pure(config))

  def floatCompare[F[_]: Sync](statusDb: GiapiStatusDb[F],
                               config:   Configuration): F[Configuration] =
    (statusDb
      .value(status)
      .map(x => x.floatCfg === config.value(configParam)))
      .ifM(Sync[F].delay(config.remove(configParam)), Sync[F].pure(config))

  def dfloatCompare[F[_]: Sync](statusDb: GiapiStatusDb[F],
                                config:   Configuration): F[Configuration] =
    (statusDb
      .value(status)
      .map(x => x.floatCfg === config.value(configParam)))
      .ifM(Sync[F].delay(config.remove(configParam)), Sync[F].pure(config))

  def stringCompare[F[_]: Sync](statusDb: GiapiStatusDb[F],
                                config:   Configuration): F[Configuration] =
    (statusDb
      .value(status)
      .map(x => x.stringCfg === config.value(configParam)))
      .ifM(Sync[F].delay(config.remove(configParam)), Sync[F].pure(config))

  def compare[F[_]: Sync](statusDb: GiapiStatusDb[F],
                          config:   Configuration): F[Configuration] = tp match {
    case "INT"    => intCompare(statusDb, config)
    case "DOUBLE" => doubleCompare(statusDb, config)
    case "FLOAT"  => floatCompare(statusDb, config)
    case "DFLOAT" => dfloatCompare(statusDb, config)
    case "STRING" => stringCompare(statusDb, config)
    case _        => sys.error("Unknown")
  }
}

object GpiStatusApply {
  val adc   = GpiStatusApply("INT", "gpi:adcDeploy", "gpi:selectAdc.deploy")
  val useAo = GpiStatusApply("INT", "gpi:ao:useAo", "gpi:configAo.useAo")
  val aoOptimize =
    GpiStatusApply("INT", "gpi:ao:optimization", "gpi:configAo.optimize")
  val useCal = GpiStatusApply("INT", "gpi:cal:useCal", "gpi:configCal.useCal")
  val fpmPinholeBias = GpiStatusApply("INT",
                                      "gpi:cal:fpmPinholeBias",
                                      "gpi:configCal.fpmPinholeBias")
  val integrationTime = GpiStatusApply("FLOAT",
                                       "gpi:currentIntegrationTime",
                                       "gpi:configIfs.integrationTime")
  val numCoadds =
    GpiStatusApply("INT", "gpi:currentNumCoadds", "gpi:configIfs.numCoadds")
  val magI =
    GpiStatusApply("FLOAT", "gpi:starIntensity", "gpi:configAo.magnitudeI")
  val magH = GpiStatusApply("FLOAT", "gpi:cal:magH", "gpi:configCal.magnitudeH")
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
  val polarizerDeploy = GpiStatusApply("INT",
                                       "gpi:polarModulatorDeploy",
                                       "gpi:configPolarizer.deploy")
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
    useAo,
    useCal,
    fpmPinholeBias,
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
    polarizerDeploy,
    observationMode,
    ifsFilter,
    ppm,
    fpm,
    lyot
  )

  def foldConfigM[F[_]: Sync](items:  List[GpiStatusApply],
                              db:     GiapiStatusDb[F],
                              config: Configuration): F[Configuration] =
    items.foldLeftM(config) { (c, i) =>
      i.compare(db, c)
    }

  def foldConfig[F[_]: Sync](db:     GiapiStatusDb[F],
                             config: Configuration): F[Configuration] =
    foldConfigM(all, db, config)

  /**
    * ObsMode needs a special treatment. It is a meta model thus it sets
    * the filter, fpm, apodizer and lyot
    * We need to check that each subsystem matches or we will
    * falsely not set the obs mode
    */
  def overrideObsMode[F[_]: Sync](db:        GiapiStatusDb[F],
                                  gpiConfig: GpiConfig,
                                  config:    Configuration): F[Configuration] =
    gpiConfig.mode match {
      case Right(_) => config.pure[F]
      case Left(o) =>
        Parsers.Gpi
          .observingMode(o.displayValue())
          .map { ob =>
            // Compare the subsystem values and the ones for the obs mode
            val filterCmp = db
              .value("gpi:ifsFilter")
              .map(x => x.stringCfg =!= ob.filter.map(_.shortName))
            val ppmCmp = db
              .value("gpi:ppmMask")
              .map(
                x =>
                  x.stringCfg =!= ob.apodizer
                    .map(_.tag)
                    .flatMap(GpiLookupTables.apodizerLUTNames.get))
            val fpmCmp = db
              .value("gpi:fpmMask")
              .map(x => x.stringCfg =!= ob.fpm.map(_.shortName))
            val lyotCmp = db
              .value("gpi:lyotMask")
              .map(x => x.stringCfg =!= ob.lyot.map(_.shortName))
            // If any doesn't match
            val subSystemsNotMatching =
              (filterCmp, ppmCmp, fpmCmp, lyotCmp).mapN(_ || _ || _ || _)
            subSystemsNotMatching.map {
              case true =>
                // force the obs mode if a subsystem doesn't match
                (config.remove("gpi:observationMode.mode") |+| Configuration
                  .single("gpi:observationMode.mode",
                          GpiLookupTables.obsModeLUT
                            .getOrElse(o, GpiLookupTables.UNKNOWN_SETTING)))
              case false =>
                config
            }
          }
          .getOrElse(config.pure[F])
    }

  val statusesToMonitor = all.map(_.status)
}
