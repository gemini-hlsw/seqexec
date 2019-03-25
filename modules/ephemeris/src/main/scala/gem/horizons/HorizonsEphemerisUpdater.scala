// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

package horizons

import gem.dao.EphemerisDao
import gem.enum.Site
import gem.math.Ephemeris
import gem.util.Timestamp
import gem.horizons.EphemerisCompression._

import cats._
import cats.effect._
import cats.implicits._

import doobie._, doobie.implicits._

import java.time.{ Duration, Period }

import fs2.Stream


/** Utility for inserting / updating en ephemeris. */
final case class HorizonsEphemerisUpdater[M[_]: Monad: LiftIO](xa: Transactor[M]) {

  import HorizonsEphemerisUpdater._

  /** Constructs an action that when run will provide the current context
    * information for an ephemeris.  This information can be used to determine
    * whether an update is needed, to see when the last update was performed,
    * and when the last update check happened.
    */
  def report(key:  EphemerisKey.Horizons, site: Site): M[EphemerisContext] =

    (for {
      meta <- EphemerisDao.selectMeta(key, site)
      rnge <- EphemerisDao.selectTimes(key, site)
      soln <- HorizonsSolutionRefQuery(key).lookup.to[ConnectionIO]
    } yield EphemerisContext(key, site, meta, rnge, soln)).transact(xa)


  /** Constructs an action that when run will insert a new ephemeris or update
    * an existing one if necessary.
    */
  def update(key: EphemerisKey.Horizons, site: Site): M[Unit] =

    for {
      time <- Timestamp.now.to[M]
      sem  <- Semester.current(site).to[M]
      ctx  <- report(key, site)
      _    <- updateIfNecessary(ctx, time, sem).transact(xa)
    } yield ()


  private def insertMeta(key: EphemerisKey.Horizons, site: Site, m: EphemerisMeta): ConnectionIO[Unit] =
    EphemerisDao.insertMeta(key, site, m).void

  private def updateMeta(key: EphemerisKey.Horizons, site: Site, m: EphemerisMeta): ConnectionIO[Unit] =
    EphemerisDao.updateMeta(key, site, m).void

  private def streamEphemeris(
    key:      EphemerisKey.Horizons,
    site:     Site,
    semester: Semester
  ): Stream[ConnectionIO, Ephemeris.Element] = {

    val qs = HorizonsEphemerisQuery.pagingSemester(key, site, semester, StepSize, Padding)

    qs.foldMap(_.streamEphemeris)
      .through(standardAccelerationCompression)
      .translate(λ[IO ~> ConnectionIO](_.to[ConnectionIO]))
  }


  private def updateIfNecessary(
    ctx:  EphemerisContext,
    time: Timestamp,
    sem:  Semester
  ): ConnectionIO[Unit] =

    if (ctx.isUpToDateFor(sem)) recordUpdateCheck(ctx, time)
    else doUpdate(ctx, time, sem)


  private def recordUpdateCheck(
    ctx:  EphemerisContext,
    time: Timestamp
  ): ConnectionIO[Unit] =

    ctx.meta.fold(().pure[ConnectionIO]) { m =>
      updateMeta(ctx.key, ctx.site, EphemerisMeta.lastUpdateCheck.set(time)(m))
    }


  private def doUpdate(
    ctx:  EphemerisContext,
    time: Timestamp,
    sem:  Semester
  ): ConnectionIO[Unit] = {

    // Need to update both meta and ephemeris.  First the new
    // metadata.
    val mʹ   = EphemerisMeta(time, time, ctx.soln)

    // The sink for writing the ephemeris to the database.  Have to pick either
    // insert or update depending upon whether there is an existing ephemeris.
    val sink = ctx.rnge.fold(EphemerisDao.streamInsert(ctx.key, ctx.site)) { _ =>
      EphemerisDao.streamUpdate(ctx.key, ctx.site)
    }

    // Update the metadata and stream the ephemeris into the database.
    for {
      _ <- ctx.meta.fold(insertMeta(ctx.key, ctx.site, mʹ)) { _ =>
             updateMeta(ctx.key, ctx.site, mʹ)
           }
      _ <- streamEphemeris(ctx.key, ctx.site, sem).through(sink).compile.drain
    } yield ()
  }

}

object HorizonsEphemerisUpdater {

  /** Padding on either side of the semester to include in an ephemeris. */
  val Padding: Period =
    Period.ofMonths(1)

  /** Maximum time between ephemeris elements, before removing points that
    * should be interpolated.
    */
  val StepSize: Duration =
    Duration.ofMinutes(1L)

}
