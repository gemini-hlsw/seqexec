// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package telnetd
package command

import gem.enum.Site

import atto._
import Atto._

import cats.data.{ NonEmptyList, ValidatedNel }
import cats.implicits._

import com.monovore.decline.Opts


/** Site options shared across various commands.
  */
trait SiteOpt {

  /** Validates a gemini-site String. */
  def validateSite(s: String): ValidatedNel[String, Site] =
    gem.EnumParsers.site.parseOnly(s)
      .either
      .leftMap(_ => s"'$s' is not a Gemini site")
      .toValidatedNel

  /** Required site option, "--site" or "-s". */
  val site: Opts[Site] =
    Opts.option[String]("site", help = "GN | GS", short = "s")
      .mapValidated(validateSite)

  /** Multiple site options, defaulting to GS and GN. */
  val sites: Opts[Set[Site]] =
    Opts.options[String]("site", help = "GN | GS", short = "s")
      .mapValidated { _.map(validateSite).sequence }
      .withDefault(NonEmptyList.of(Site.GN, Site.GS))
      .map { nel => Set(nel.toList: _*) }

}
