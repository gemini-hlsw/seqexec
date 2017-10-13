// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

package horizons

import gem.dao.EphemerisDao
import gem.enum.Site
//import gem.math.Ephemeris
import gem.util.InstantMicros

import cats.effect.IO
import cats.implicits._

import doobie._, doobie.implicits._

//import fs2.Stream


/**
  */
trait HorizonsEphemerisUpdater {

  private val Url  = "jdbc:postgresql:gem"
  private val User = "postgres"
  private val Pass = ""

  private val xa = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver", Url, User, Pass
  )

  private def recordUpdateCheck(k: EphemerisKey, s: Site, m: EphemerisMeta, t: InstantMicros): IO[Unit] = {
    val mʹ = EphemerisMeta.lastUpdateCheck.set(t)(m)
    EphemerisDao.updateMeta(k, s, mʹ).void.transact(xa)
  }

  private def needsUpdate(m: EphemerisMeta, os: Option[HorizonsSolutionRef]): Boolean =
    (for {
      s0  <- m.solnRef
      s1  <- os
    } yield s0 =!= s1).getOrElse(true)

  /*
  private def query(
                key:      EphemerisKey.Horizons,
                site:     Site,
                semester: Semester): Stream[ConnectionIO, Ephemeris.Element] = {
    println(s"key      = $key")
    println(s"site     = $site")
    println(s"semester = $semester")

    ???
  }
  */

//    Monoid.combineAll(subQueries(key, site, semester).map(_.streamEphemeris))
//      .through(standardVelocityCompression)
//      .translateSync(λ[IO ~> ConnectionIO](_.liftIO[ConnectionIO]))


  private def updateAction(
                key:      EphemerisKey.Horizons,
                site:     Site,
                semester: Semester,
                soln:     Option[HorizonsSolutionRef],
                time:     InstantMicros): IO[Unit] = {

    println(s"key      = $key")
    println(s"site     = $site")
    println(s"semester = $semester")
    println(s"soln     = $soln")
    println(s"time     = $time")

//    query(key, site, semester).to(EphemerisDao.streamUpdate(key, site)).run.unsafeRunSync

    IO.pure(())
  }

  def updateEphemeris(key: EphemerisKey.Horizons, site: Site, semester: Semester): IO[Unit] =
    for {
//      u <- UserDao.selectRootUser.transact(xa)
//      l <- Log.newLog[ConnectionIO]
      t <- InstantMicros.now
      m <- EphemerisDao.selectMeta(key, site).transact(xa)
      s <- HorizonsSolutionRefQuery(key).lookup
      a  = updateAction(key, site, semester, s, t)
      _ <- m.fold(a) { meta =>
             if (needsUpdate(meta, s)) a
             else recordUpdateCheck(key, site, meta, t)
           }
    } yield ()

}

object HorizonsEphemerisUpdater extends HorizonsEphemerisUpdater
