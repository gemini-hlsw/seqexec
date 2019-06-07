// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import doobie._
import doobie.implicits._
import gem.dao.meta._
import gem.dao.composite._
import gem.enum.TrackType
import gsp.math.{ Declination, ProperMotion, RightAscension }

object TargetDao extends EnumeratedMeta /* extend EnumeratedMeta to lower the priority - see MetaTrackType below and issue #170 */ {

  import EphemerisKeyComposite._
  import ProperMotionComposite._

  // MetaTrackType is a workaround until issue #170 is implemented.
  import doobie.postgres.implicits._
  implicit val MetaTrackType: Meta[TrackType] =
    pgEnumString("e_track_type", TrackType.unsafeFromTag, _.tag)

  def select(id: Target.Id): ConnectionIO[Option[Target]] =
    Statements.select(id).option

  def selectUnique(id: Target.Id): ConnectionIO[Target] =
    Statements.select(id).unique

  def insert(target: Target): ConnectionIO[Target.Id] =
    Statements.insert(target)
              .withUniqueGeneratedKeys[Int]("id")
              .map(Target.Id(_))

  def update(id: Target.Id, target: Target): ConnectionIO[Int] =
    Statements.update(id, target).run

  def delete(id: Target.Id): ConnectionIO[Int] =
    Statements.delete(id).run

  object Statements {

    private final case class TargetKernel(
      name: String,
      trackType: TrackType,
      ephemerisKey: Option[EphemerisKey],
      properMotion: Option[ProperMotion]
    ) {

      def toTarget: Target =
        trackType match {
          case TrackType.Nonsidereal =>
            Target(name, ephemerisKey.toLeft(sys.error("missing ephemeris key")))
          case TrackType.Sidereal    =>
            Target(name, properMotion.toRight(sys.error("missing proper motion")))
        }

    }

    private def trackType(t: Target): TrackType =
      t.track.fold(_ => TrackType.Nonsidereal, _ => TrackType.Sidereal)

    // base coordinates formatted as readable strings, if target is sidereal
    private def stringyCoordinates(t: Target): Option[(String, String)] =
      t.track.toOption.map { pm =>
        val cs = pm.baseCoordinates
        (RightAscension.fromStringHMS.reverseGet(cs.ra),
         Declination.fromStringSignedDMS.reverseGet(cs.dec))
      }

    def select(id: Target.Id): Query0[Target] =
      sql"""
        SELECT name, track_type,
               e_key_type, e_key,                     -- ephemeris key
               ra, dec, epoch, pv_ra, pv_dec, rv, px  -- proper motion
          FROM target
         WHERE id = $id
      """.query[TargetKernel].map(_.toTarget)

    def insert(target: Target): Update0 =
      (fr"""INSERT INTO target (
              name, track_type,
              e_key_type, e_key,                     -- ephemeris key
              ra, dec, epoch, pv_ra, pv_dec, rv, px, -- proper motion
              ra_str, dec_str                        -- stringy coordinates
           ) VALUES""" ++ values(
             (target.name, trackType(target),
              target.track.left.toOption,
              target.track.right.toOption,
              stringyCoordinates(target)
             )
           )
      ).update

    def update(id: Target.Id, target: Target): Update0 =
      (fr"""UPDATE target
            SET (name, track_type,
                 e_key_type, e_key,                     -- ephemeris key
                 ra, dec, epoch, pv_ra, pv_dec, rv, px, -- proper motion
                 ra_str, dec_str                        -- stringy coordinates
            ) =""" ++ values(
            (target.name, trackType(target),
             target.track.left.toOption,
             target.track.right.toOption,
             stringyCoordinates(target)
            )
      ) ++
       fr"WHERE id = $id").update

    def delete(id: Target.Id): Update0 =
      sql"DELETE FROM target WHERE id=$id".update

  }

}
