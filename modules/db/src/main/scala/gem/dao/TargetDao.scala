// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import doobie._
import doobie.implicits._
import gem.dao.meta._
import gem.dao.composite._
import gem.enum.TrackType
import shapeless._

object TargetDao {
  import EnumeratedMeta._
  import EphemerisKeyComposite._
  import ProperMotionComposite._
  import TaggedCoproduct._
  import Track._

  def select(id: Int): ConnectionIO[Option[Target]] =
    Statements.select(id).option

  def insert(target: Target): ConnectionIO[Int] =
    Statements.insert(target).withUniqueGeneratedKeys[Int]("id")

  def update(id: Int, target: Target): ConnectionIO[Int] =
    Statements.update(id, target).run

  def delete(id: Int): ConnectionIO[Int] =
    Statements.delete(id).run

  object Statements {

    // Track is laid out as a tagged coproduct: (tag, sidereal, nonsidereal).
    private implicit val TaggedTrackComposite: Composite[Track] = {

      // We map only the ephemeris key portion of the nonsidereal target here, and we only need to
      // consider the Option[Nonsidereal] case because this is what the coproduct encoding needs.
      implicit val compositeOptionNonsidereal: Composite[Option[Nonsidereal]] =
        Composite[Option[EphemerisKey]].imap(_.map(Nonsidereal.empty))(_.map(_.ephemerisKey))

      // Construct an encoder for track constructors, tagged by TrackType.
      val enc = Tag[Sidereal](TrackType.Sidereal)       :+:
                Tag[Nonsidereal](TrackType.Nonsidereal) :+: TNil

      // from enc we get a Composite[Sidereal :+: Nonsidereal :+: CNil], which we map out to Track
      enc.composite.imap(_.unify) {
        case t: Sidereal    => enc.inj(t)
        case t: Nonsidereal => enc.inj(t)
      }

    }

    // base coordinates formatted as readable strings, if target is sidereal
    private def stringyCoordinates(t: Target): Option[(String, String)] =
      t.track.sidereal.map { st =>
        val cs = st.properMotion.baseCoordinates
        (cs.ra.format, cs.dec.format)
      }

    def select(id: Int): Query0[Target] =
      sql"""
        SELECT name, track_type,
               ra, dec, epoch, pv_ra, pv_dec, rv, px, -- proper motion
               e_key_type, e_key                      -- ephemeris key
          FROM target
         WHERE id = $id
      """.query[Target]

    def insert(target: Target): Update0 =
      (fr"""INSERT INTO target (
              name, track_type,
              ra, dec, epoch, pv_ra, pv_dec, rv, px, -- proper motion
              e_key_type, e_key,                     -- ephemeris key
              ra_str, dec_str                        -- stringy coordinates
           ) VALUES""" ++ values((target, stringyCoordinates(target)))).update

    def update(id: Int, target: Target): Update0 =
      (fr"""UPDATE target
            SET (name, track_type,
                 ra, dec, epoch, pv_ra, pv_dec, rv, px, -- proper motion
                 e_key_type, e_key,                     -- ephemeris key
                 ra_str, dec_str                        -- stringy coordinates
            ) =""" ++ values((target, stringyCoordinates(target))) ++
       fr"WHERE id = $id").update

    def delete(id: Int): Update0 =
      sql"DELETE FROM target WHERE id=$id".update

  }

}
