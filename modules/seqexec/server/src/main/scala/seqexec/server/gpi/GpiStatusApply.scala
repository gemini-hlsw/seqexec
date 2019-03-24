// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import cats.implicits._
import cats.effect.Sync
import gem.enum.GiapiStatusApply
import gem.enum.GiapiStatusApply._
import gem.enum.GiapiType
import gem.enum.Instrument
import gem.ocs2.Parsers
import giapi.client.commands.Configuration
import giapi.client.GiapiStatusDb
import giapi.client.StatusValue
import giapi.client.syntax.status._
import seqexec.server.gpi.GpiController.GpiConfig

object GpiStatusApply {
  val allGpi: List[GiapiStatusApply] = GiapiStatusApply.all.filter {
    _.instrument === Instrument.Gpi
  }

  val statusesToMonitor = allGpi.map(_.statusItem)

  def foldConfigM[F[_]: Sync](items:  List[GiapiStatusApply],
                              db:     GiapiStatusDb[F],
                              config: Configuration): F[Configuration] =
    items.foldLeftM(config) { (c, i) =>
      i.removeConfigItem(db, c)
    }

  def foldConfig[F[_]: Sync](db:     GiapiStatusDb[F],
                             config: Configuration): F[Configuration] =
    foldConfigM(allGpi, db, config)

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
              .value(GpiIFSFilter.statusItem)
              .map(x => x.stringCfg =!= ob.filter.map(_.shortName))

            val ppmCmp = db
              .value(GpiPPM.statusItem)
              .map(
                x =>
                  x.stringCfg =!= ob.apodizer
                    .map(_.tag)
                    .flatMap(GpiLookupTables.apodizerLUTNames.get))

            val fpmCmp = db
              .value(GpiFPM.statusItem)
              .map(x => x.stringCfg =!= ob.fpm.map(_.shortName))

            val lyotCmp = db
              .value(GpiLyot.statusItem)
              .map(x => x.stringCfg =!= ob.lyot.map(_.shortName))

            // If any doesn't match
            val subSystemsNotMatching =
              (filterCmp, ppmCmp, fpmCmp, lyotCmp).mapN(_ || _ || _ || _)

            subSystemsNotMatching.map {
              case true =>
                // force the obs mode if a subsystem doesn't match
                (config.remove(GpiObservationMode.applyItem) |+| Configuration
                  .single(GpiObservationMode.applyItem,
                          GpiLookupTables.obsModeLUT
                            .getOrElse(o, GpiLookupTables.UNKNOWN_SETTING)))
              case false =>
                config
            }
          }
          .getOrElse(config.pure[F])
    }

  implicit class GiapiStatusApplyOps(val s: GiapiStatusApply) {
    private def removeConfigIfPossible[F[_]: Sync](
      statusDb: GiapiStatusDb[F],
      config:   Configuration,
      compare:  Option[StatusValue] => Option[String]
    ): F[Configuration] =
      statusDb
        .value(s.statusItem)
        .map(v => compare(v) === config.value(s.applyItem))
        .ifM(Sync[F].delay(config.remove(s.applyItem)), Sync[F].pure(config))

    def removeConfigItem[F[_]: Sync](statusDb: GiapiStatusDb[F],
                                     config:   Configuration): F[Configuration] =
      s.statusType match {
        case GiapiType.Int =>
          removeConfigIfPossible(statusDb, config, _.intCfg)
        case GiapiType.Double =>
          removeConfigIfPossible(statusDb, config, _.doubleCfg)
        case GiapiType.Float =>
          removeConfigIfPossible(statusDb, config, _.floatCfg)
        case GiapiType.String =>
          removeConfigIfPossible(statusDb, config, _.stringCfg)
      }
  }
}
