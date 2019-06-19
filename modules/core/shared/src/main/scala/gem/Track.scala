// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.Eq

import gem.enum.Site
import gem.math.Ephemeris
import gem.util.Timestamp
import gsp.math._

import java.time.Instant

import monocle.{ Optional, Prism }
import monocle.macros.{ Lenses, GenPrism }

/**
 * Time/site-parameterized coordinates over a span of time. This generalizes proper motion and
 * ephemerides.
 */
sealed trait Track extends Product with Serializable {
  import Track._

  def at(time: Instant, site: Site): Option[Coordinates]

  def fold[A](f: ProperMotion => A, g: (Map[Site, Ephemeris]) => A): A =
    this match {
      case Sidereal(pm)    => f(pm)
      case Nonsidereal(es) => g(es)
    }

  def sidereal: Option[Sidereal] =
    fold(pm => Some(Sidereal(pm)), _ => None)

  def nonsidereal: Option[Nonsidereal] =
    fold(_ => None, (es) => Some(Nonsidereal(es)))

}

object Track {

  @Lenses final case class Sidereal(properMotion: ProperMotion) extends Track {
    override def at(time: Instant, site: Site): Option[Coordinates] =
      Some(properMotion.at(time).baseCoordinates)
  }

  object Sidereal {

    implicit val EqSidereal: Eq[Sidereal] =
      Eq.fromUniversalEquals
  }


  @Lenses final case class Nonsidereal(ephemerides: Map[Site, Ephemeris]) extends Track {

    override def at(time: Instant, s: Site): Option[Coordinates] =
      for {
        i <- Timestamp.fromInstant(time)
        e <- ephemeris(s)
        c <- e.get(i)
      } yield c.coord

    def ephemeris(s: Site): Option[Ephemeris] =
      ephemerides.get(s)

  }

  object Nonsidereal {

    val empty: Nonsidereal =
      Nonsidereal(Map.empty)

    implicit val EqNonsidereal: Eq[Nonsidereal] =
      Eq.fromUniversalEquals

  }

  val sidereal: Prism[Track, Sidereal] =
    GenPrism[Track, Sidereal]

  val nonsidereal: Prism[Track, Nonsidereal] =
    GenPrism[Track, Nonsidereal]

  val ephemerides: Optional[Track, Map[Site, Ephemeris]] =
    nonsidereal composeLens Nonsidereal.ephemerides

  implicit val EqTrack: Eq[Track] =
    Eq.fromUniversalEquals
}
