// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.enum.Site
import gem.math._
import gem.util.InstantMicros
import java.time.Instant

/**
 * Time/site-parameterized coordinates over a span of time. This generalizes proper motion and
 * ephemerides.
 */
sealed trait Track extends Product with Serializable {
  import Track._

  def at(time: Instant, site: Site): Option[Coordinates]

  def fold[A](f: ProperMotion => A, g: (EphemerisKey, Map[Site, Ephemeris]) => A): A =
    this match {
      case Sidereal(pm)       => f(pm)
      case Nonsidereal(k, es) => g(k, es)
    }

  def sidereal: Option[Sidereal] =
    fold(pm => Some(Sidereal(pm)), (_, _) => None)

  def nonsidereal: Option[Nonsidereal] =
    fold(_ => None, (k, es) => Some(Nonsidereal(k, es)))

}

object Track {

  final case class Sidereal(properMotion: ProperMotion) extends Track {
    override def at(time: Instant, site: Site) =
      Some(properMotion.at(time).baseCoordinates)
  }

  final case class Nonsidereal(ephemerisKey: EphemerisKey, ephemerides: Map[Site, Ephemeris]) extends Track {

    override def at(time: Instant, s: Site) =
      ephemeris(s).flatMap(_.get(InstantMicros.truncate(time)))

    def ephemeris(s: Site): Option[Ephemeris] =
      ephemerides.get(s)

  }
  object Nonsidereal {

    def empty(key: EphemerisKey): Nonsidereal =
      Nonsidereal(key, Map.empty)

  }

}
