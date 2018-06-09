// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package telnetd
package command

import gem.enum.Site
import gem.horizons.EphemerisContext
import gem.math.ObservingNight
import gem.util.Timestamp

import cats.data.NonEmptyList
import cats.implicits._
import com.monovore.decline.{ Command => _, _ }
import tuco._
import Tuco._
import tuco.shell._

import java.nio.file.Path
import java.time.Instant
import java.time.ZoneOffset.UTC
import java.time.format.DateTimeFormatter

/** Commands for working with ephemerides.
  *
  * exportCommand - exports ephemerides to files for the TCS
  * reportCommand - display ephemerides status information
  * updateCommand - updates ephemerides from horizons if necessary
  */
object eph extends SiteOpt {

  /** Export directory option. */
  val dir: Opts[Path] =
    Opts.option[Path]("dir", help = "export to directory", short = "d")
      .validate("Directory must be writeable") { _.toFile.canWrite }

  /** Ephemeris key argument parsing.
    *
    * @param name     name of the ephemeris key type (e.g., "horizons-key", or
    *                 generic "ephemeris-key"
    * @param failMsg  text to display if a key cannot be parsed
    * @param collectf function that selects a subset of valid ephemeris keys
    *                 (e.g., only those that are also horizons keys)
    */
  private def keys[K <: EphemerisKey](
    name:    String,
    failMsg: String => String
  )(
    collectf: PartialFunction[EphemerisKey, K]
  ): Opts[NonEmptyList[K]] =

    Opts.arguments[String](name)
      .orEmpty
      .mapValidated {
        _.foldMap { keyStr =>
          EphemerisKey.fromString
            .getOption(keyStr)
            .collect(collectf)
            .fold(failMsg(keyStr).invalidNel[List[K]]) { k =>
              List(k).validNel[String]
            }
        }.withEither(_.flatMap {
          case Nil    => Left(NonEmptyList.one("Must supply at least one valid key"))
          case h :: t => Right(NonEmptyList.of(h, t: _*))
        })
      }

  /** Argument for one or more ephemeris keys of any type. */
  val anyKeys: Opts[NonEmptyList[EphemerisKey]] =
    keys("ephemeris-key", keyStr => s"'$keyStr' is not a valid ephemeris key") {
      case k => k
    }

  /** Argument for one or more horizons keys. */
  val horizonsKeys: Opts[NonEmptyList[EphemerisKey.Horizons]] =
    keys("horizons-key", keyStr => s"'$keyStr' is not a valid horizons key") {
      case k: EphemerisKey.Horizons => k
    }

  /** Command that exports ephemeris files in TCS format to a directory on the
    * server.
    */
  val exportCommand: GemCommand = {
    def range(now: Instant, s: Site): (Timestamp, Timestamp) = {
      val n = ObservingNight.fromSiteAndInstant(s, now)
      (Timestamp.unsafeFromInstant(n.start), Timestamp.unsafeFromInstant(n.end))
    }

    Command(
      "eph-export", "Export ephemerides for the current night for use by the TCS",
      (site, dir, anyKeys).mapN { (s: Site, p: Path, ks: NonEmptyList[EphemerisKey]) =>
        (d: GemState) => {

          ks.traverse { k =>
            for {
              r <- SessionIO.delay(Instant.now).map(range(_, s))
              _ <- write(s"Exporting $k @ $s ... ")
              f <- d.ephemeris.export(k, s, r._1, r._2, p)
              _ <- writeLn(s"$f")
            } yield ()
          }.as(d)
        }
      }
    ).zoom(Session.data[GemState])

  }

  private def horizonsCommand(name: String, help: String, msg: String)
                             (f: (GemState, EphemerisKey.Horizons, Site) => SessionIO[EphemerisContext]): GemCommand =
    Command(
      name, help,
      (site, horizonsKeys).mapN { (s: Site, ks: NonEmptyList[EphemerisKey.Horizons]) => (d: GemState) =>
        ks.traverse { k =>
          for {
            _ <- writeLn(s"$msg $k @ $s ...")
            c <- f(d, k, s)
            _ <- writeLn(report.format(c))
          } yield ()
        }.as(d)
      }
    ).zoom(Session.data[GemState])

  val reportCommand: GemCommand =
    horizonsCommand("eph-report", "Display ephemeris report", "Looking up") { (d, k, s) =>
      d.ephemeris.report(k, s)
    }

  val updateCommand: GemCommand =
    horizonsCommand("eph-update", "Update ephemeris from horizons, if necessary", "Updating") { (d, k, s) =>
      d.ephemeris.update(k, s)
    }

  /** EphemerisContext report formatting. */
  private object report {
    val timeFormatter: DateTimeFormatter =
      DateTimeFormatter.RFC_1123_DATE_TIME.withZone(UTC)

    def formatTime(ot: Option[Timestamp]): String =
      ot.map(t => timeFormatter.format(t.toInstant)).orEmpty

    /** Formats an EphemerisContext for display to the user. */
    def format(ctx: EphemerisContext): String =
      s"""
         |${ctx.key} @ ${ctx.site}
         |  Last Update.......: ${formatTime(ctx.meta.map(_.lastUpdate))}
         |  Last Update Check.: ${formatTime(ctx.meta.map(_.lastUpdateCheck))}
         |  Solution Reference: ${ctx.meta.flatMap(_.solnRef).map(_.stringValue).orEmpty}
         |  Available Range...: ${formatTime(ctx.rnge.map(_._1))}${ctx.rnge.as(" - ").orEmpty}${formatTime(ctx.rnge.map(_._2))}
       """.stripMargin
  }

}
