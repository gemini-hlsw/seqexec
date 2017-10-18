// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.{ EphemerisKey, Log }
import gem.dao.UserDao
import gem.enum.Site

import cats.effect.IO
import cats.implicits._

import doobie._, doobie.implicits._


/** A simple test application that drives ephemeris updates.
  *
  * {{{
  *   > ephemeris/test:run gem.horizons.HorizonsUpdater site key ...
  * }}}
  *
  * e.g.,
  *
  * {{{
  *   ephemeris/test:run gem.horizons.HorizonsUpdater GS MajorBody_605 MajorBody_606
  * }}}
  */
object HorizonsUpdaterApp {

  private val Url  = "jdbc:postgresql:gem"
  private val User = "postgres"
  private val Pass = ""

  private val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver", Url, User, Pass
  )

  private def putStrLn(msg: String): IO[Unit] =
    IO(println(msg))

  type Parsed[A] = Either[String, A]

  final case class Args(site: Site, keys: List[EphemerisKey.Horizons])

  object Args {

    def parse(args: List[String]): Parsed[Args] = {

      def parseSite(siteStr: String): Parsed[Site] =
        Site.fromTag(siteStr) match {
          case Some(site) => Either.right(site)
          case None       => Either.left(s"Expecting GN or GS, not '$siteStr'")
        }

      def parseKeys(keys: List[String]): Parsed[List[EphemerisKey.Horizons]] =
        keys.traverse { keyStr =>
          EphemerisKey.parse(keyStr) match {
            case Some(key: EphemerisKey.Horizons) => Either.right(key)
            case Some(key)                        => Either.left(s"'$keyStr' is not an horizons key")
            case None                             => Either.left(s"Could not parse '$keyStr' as an horizons key")
          }
        }

      args.drop(1).toList match {

        case siteStr :: keys =>
          for {
            s  <- parseSite(siteStr)
            ks <- parseKeys(keys)
          } yield Args(s, ks)

        case _ =>
          Either.left("usage: HorizonsUpdaterApp [GN|GS] ephemeris_key...")
      }
    }
  }

  def main(args: Array[String]): Unit =

    (Args.parse(args.toList) match {

      case Left(msg)  =>
        putStrLn(msg)

      case Right(cmd) =>
        for {
          user <- UserDao.selectRootUser.transact(xa)
          log  <- Log.newLogIn[ConnectionIO, IO]("HorizonsEphemerisUpdater", xa)
          up    = HorizonsEphemerisUpdater(user, log, xa)
          _    <- cmd.keys.traverse(up.update(_, cmd.site))
          _    <- log.shutdown(5 * 1000).transact(xa)
        } yield ()
      }

    ).unsafeRunSync

}
