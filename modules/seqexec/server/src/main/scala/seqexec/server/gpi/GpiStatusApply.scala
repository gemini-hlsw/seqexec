// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import cats._
import cats.implicits._
import gem.enum.GiapiStatusApply
import gem.enum.GiapiStatusApply._
import gem.enum.GiapiStatus
import gem.enum.GiapiType
import gem.enum.Instrument
import gem.ocs2.Parsers
import gsp.math.Angle
import gsp.math.syntax.all._
import giapi.client.commands.Configuration
import giapi.client.GiapiStatusDb
import giapi.client.StatusValue
import giapi.client.syntax.status._

object GpiStatusApply extends GpiLookupTables {
  val allGpiApply: List[GiapiStatusApply] = GiapiStatusApply.all.filter {
    _.instrument === Instrument.Gpi
  }

  val allGpiStatus: List[GiapiStatus] = GiapiStatus.all.filter {
    _.instrument === Instrument.Gpi
  }

  val statusesToMonitor = allGpiApply.map(_.statusItem) ++
    allGpiStatus.map(_.statusItem)

  def foldConfigM[F[_]: Monad](
    items:  List[GiapiStatusApply],
    db:     GiapiStatusDb[F],
    config: Configuration
  ): F[Configuration] =
    items.foldLeftM(config) { (c, i) =>
      i.removeConfigItem(db, c)
    }

  def foldConfig[F[_]: Monad](
    db:     GiapiStatusDb[F],
    config: Configuration
  ): F[Configuration] =
    foldConfigM(allGpiApply, db, config)

  /**
    * ObsMode needs a special treatment. It is a meta model thus it sets
    * the filter, fpm, apodizer and lyot
    * We need to check that each subsystem matches or we will
    * falsely not set the obs mode
    */
  def overrideObsMode[F[_]: Monad](
    db:        GiapiStatusDb[F],
    gpiConfig: RegularGpiConfig,
    config:    Configuration
  ): F[Configuration] =
    gpiConfig.mode match {
      case Right(_) => config.pure[F]
      case Left(o) =>
        Parsers.Gpi
          .observingMode(o.displayValue())
          .map { ob =>
            // Compare the subsystem values and the ones for the obs mode
            val filterCmp = db
              .optional(GpiIFSFilter.statusItem)
              .map(x => x.stringCfg =!= ob.filter.map(_.shortName))

            val ppmCmp = db
              .optional(GpiPPM.statusItem)
              .map(
                x =>
                  x.stringCfg =!= ob.apodizer
                    .map(_.tag)
                    .flatMap(apodizerLUTNames.get)
              )

            val fpmCmp = db
              .optional(GpiFPM.statusItem)
              .map(x => x.stringCfg =!= ob.fpm.map(_.shortName))

            val lyotCmp = db
              .optional(GpiLyot.statusItem)
              .map(x => x.stringCfg =!= ob.lyot.map(_.shortName))

            // If any doesn't match
            val subSystemsNotMatching =
              (filterCmp, ppmCmp, fpmCmp, lyotCmp).mapN(_ || _ || _ || _)

            subSystemsNotMatching.map {
              case true =>
                // force the obs mode if a subsystem doesn't match
                (config.remove(GpiObservationMode.applyItem) |+| Configuration
                  .single(GpiObservationMode.applyItem,
                          obsModeLUT.getOrElse(o, UNKNOWN_SETTING)))
              case false =>
                config
            }
          }
          .getOrElse(config.pure[F])
    }

  implicit class GiapiStatusApplyOps(val s: GiapiStatusApply) {
    def removeConfig[F[_]: Monad](
      statusDb: GiapiStatusDb[F],
      config:   Configuration,
      compare:  Option[StatusValue] => Boolean
    ): F[Configuration] =
      statusDb
        .optional(s.statusItem)
        .map(compare)
        .ifM(config.remove(s.applyItem).pure[F], config.pure[F])

    private def removeConfigIfPossible[F[_]: Monad](
      statusDb: GiapiStatusDb[F],
      config:   Configuration,
      compare:  Option[StatusValue] => Option[String]
    ): F[Configuration] =
      removeConfig(statusDb, config, x => compare(x) === config.value(s.applyItem))

    private def removeConfigIfPossible[F[_]: Monad, A: Numeric](
      statusDb: GiapiStatusDb[F],
      config:   Configuration,
      compare:  Option[StatusValue] => Option[A],
      toA:      String => Option[A],
      tolerance: A
    ): F[Configuration] = {
      val num = implicitly[Numeric[A]]
      removeConfig(statusDb, config,
        x => compare(x).exists{v =>
          val configValue = config.value(s.applyItem).flatMap(toA)
          configValue.exists{ c =>
          val diff = num.abs(num.minus(v, c))
          num.lteq(diff, tolerance)
        }})
    }

    def removeConfigItem[F[_]: Monad](
      statusDb: GiapiStatusDb[F],
      config:   Configuration
    ): F[Configuration] =
      s.statusType match {
        case GiapiType.Int =>
          removeConfigIfPossible(statusDb, config, _.intCfg.flatMap(_.parseIntOption), _.parseIntOption, s.tolerance.foldMap(_.toInt))
        case GiapiType.Double =>
          removeConfigIfPossible[F, Double](statusDb, config, _.doubleCfg.flatMap(_.parseDoubleOption), _.parseDoubleOption, s.tolerance.foldMap(_.toDouble))
        case GiapiType.Float =>
          removeConfigIfPossible[F, Float](statusDb, config, _.floatCfg.flatMap(_.parseDoubleOption.map(_.toFloat)), _.parseDoubleOption.map(_.toFloat), s.tolerance.foldMap(_.toFloat))
        case GiapiType.String =>
          removeConfigIfPossible(statusDb, config, _.stringCfg)
      }
  }

  /**
    * polarizer angle needs a special treatment.
    */
  def overridePolAngle[F[_]: Monad](
    db:        GiapiStatusDb[F],
    config:    Configuration
  ): F[Configuration] =
    GpiPolarizerAngle.removeConfig[F](
      db,
      config,
      value => {
        val measuredAngle: Option[Angle] =
          value.floatCfg.flatMap(_.parseDoubleOption).map(Angle.fromDoubleDegrees)
        val requestedAngle: Option[Angle] =
          config.value(GpiPolarizerAngle.applyItem).flatMap(_.parseDoubleOption).map(Angle.fromDoubleDegrees)
        (measuredAngle, requestedAngle).mapN {(m, r) =>
          implicit val order: Order[Angle] = Angle.AngleOrder
          val δ: Angle = m.difference(r)
          val ε: Option[Angle] = GpiPolarizerAngle.tolerance.map(t => Angle.fromDoubleDegrees(t.toDouble))
          ε.exists(δ <= _)
        }.getOrElse(false)
      })


}
