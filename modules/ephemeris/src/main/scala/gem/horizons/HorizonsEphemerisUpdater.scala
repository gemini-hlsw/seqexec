// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

package horizons

import gem.dao.EphemerisDao
import gem.enum.Site
import gem.math.Ephemeris
import gem.util.InstantMicros
import gem.horizons.VelocityCompression._

import cats._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._

import doobie._, doobie.implicits._

import java.time.{ Duration, Period }

import fs2.Stream

import scala.math.Ordering.Implicits._

/** Utility for inserting / updating en ephemeris. */
final case class HorizonsEphemerisUpdater(
  user: User[_],
  log:  Log[ConnectionIO],
  xa:   Transactor[IO]) {

  import HorizonsEphemerisUpdater._

  /** Constructs an action that when run will provide the current context
    * information for an ephemeris.  This information can be used to determine
    * whether an update is needed, to see when the last update was performed,
    * and when the last update check happened.
    */
  def context(key:  EphemerisKey.Horizons, site: Site): IO[Context] =

    (for {

      meta <- log.log(user, s"EphemerisDao.selectMeta($key, $site)") {
                EphemerisDao.selectMeta(key, site)
              }

      rnge <- log.log(user, s"EphemerisDao.selectTimes($key, $site)") {
                EphemerisDao.selectTimes(key, site)
              }

      soln <- log.log(user, s"HorizonsSolutionRefQuery($key).lookup") {
                HorizonsSolutionRefQuery(key).lookup.liftIO[ConnectionIO]
              }

    } yield Context(key, site, meta, rnge, soln)).transact(xa)


  /** Constructs an action that when run will insert a new ephemeris or update
    * an existing one if necessary.
    */
  def update(key: EphemerisKey.Horizons, site: Site): IO[Unit] =

    for {

      time <- InstantMicros.now
      sem  <- Semester.current(site)
      ctx  <- context(key, site)
      _    <- updateIfNecessary(ctx, time, sem).transact(xa)

    } yield ()


  private def logMessage(msg: String): ConnectionIO[Unit] =
    log.log(user, msg)(().pure[ConnectionIO])

  private def insertMeta(key: EphemerisKey.Horizons, site: Site, m: EphemerisMeta): ConnectionIO[Unit] =
    log.log(user, s"insertMeta($key, $site, $m)") {
      EphemerisDao.insertMeta(key, site, m).void
    }

  private def updateMeta(key: EphemerisKey.Horizons, site: Site, m: EphemerisMeta): ConnectionIO[Unit] =
    log.log(user, s"updateMeta($key, $site, $m)") {
      EphemerisDao.updateMeta(key, site, m).void
    }

  private def streamEphemeris(
    key:      EphemerisKey.Horizons,
    site:     Site,
    semester: Semester
  ): Stream[ConnectionIO, Ephemeris.Element] = {

    val qs = HorizonsEphemerisQuery.pagingSemester(key, site, semester, StepSize, Padding)

    qs.foldMap(_.streamEphemeris)
      .through(standardVelocityCompression)
      .translateSync(λ[IO ~> ConnectionIO](_.liftIO[ConnectionIO]))
  }


  private def updateIfNecessary(
    ctx:  Context,
    time: InstantMicros,
    sem:  Semester
  ): ConnectionIO[Unit] =

    if (ctx.isUpToDateFor(sem)) recordUpdateCheck(ctx, time)
    else doUpdate(ctx, time, sem)


  private def recordUpdateCheck(
    ctx:  Context,
    time: InstantMicros
  ): ConnectionIO[Unit] =

    for {
      _ <- logMessage(s"${ctx.key}@${ctx.site} already up-to-date. Record update check.")
      _ <- ctx.meta.fold(().pure[ConnectionIO]) { m =>
            updateMeta(ctx.key, ctx.site, EphemerisMeta.lastUpdateCheck.set(time)(m))
          }
    } yield ()


  private def doUpdate(
    ctx:  Context,
    time: InstantMicros,
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
      _ <- logMessage(s"Update ${ctx.key}@${ctx.site}")
      _ <- ctx.meta.fold(insertMeta(ctx.key, ctx.site, mʹ)) { _ =>
             updateMeta(ctx.key, ctx.site, mʹ)
           }
      _ <- log.log(user, s"streamEphemeris(${ctx.key}, ${ctx.site}, $sem)") {
             streamEphemeris(ctx.key, ctx.site, sem).to(sink).run
           }
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

  /** Collection of information related to an ephemeris.  Used to determine
    * whether an update is needed.
    *
    * @param key  unique horizons id of the object
    * @param site information valid for the given site
    * @param meta associated ephemeris metadata, if any
    * @param rnge earliest and latest ephemeris element times, if any
    * @param soln current horizons solution reference from JPL, if any
    */
  final case class Context(
    key:  EphemerisKey.Horizons,
    site: Site,
    meta: Option[EphemerisMeta],
    rnge: Option[(InstantMicros, InstantMicros)],
    soln: Option[HorizonsSolutionRef]) {

    /** Determines whether the existing ephemeris, if any, covers the given
      * semester.
      */
    def coversSemester(sem: Semester): Boolean =
      rnge.exists { case (start, end) =>
        (start.toInstant <= sem.start.atSite(site).toInstant) &&
          (sem.end.atSite(site).toInstant <= end.toInstant)
      }

    /** Whether the database and latest horizons version data matches. */
    val sameSolution: Boolean =
      (meta.flatMap(_.solnRef), soln).tupled.exists { case (s0, s1) =>
        s0 === s1
      }

    /** Determines whether updates are needed to obtain the latest ephemeris for
      * the given semester.
      */
    def isUpToDateFor(sem: Semester): Boolean =
      coversSemester(sem) && sameSolution
  }

}
