// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.dao.composite._
import gem.dao.meta._
import gem.enum.Site
import gem.math.{ EphemerisCoordinates, Ephemeris }
import gem.util.Timestamp
import gsp.math.{ Coordinates, Declination, Offset, RightAscension }

import cats.implicits._
import doobie._, doobie.implicits._
import fs2.{ Pipe, Stream }

object EphemerisDao {
  import CoordinatesComposite._
  import EnumeratedMeta._
  import EphemerisKeyComposite._
  import OffsetMeta._
  import TimeMeta._

  private def toRow(k: EphemerisKey, s: Site, i: Timestamp, c: EphemerisCoordinates): Statements.EphemerisRow =
    (k,
     s,
     i,
     c.coord,
     RightAscension.fromStringHMS.reverseGet(c.coord.ra),
     Declination.fromStringSignedDMS.reverseGet(c.coord.dec),
     c.delta
    )

  def insert(k: EphemerisKey, s: Site, e: Ephemeris): ConnectionIO[Int] =
    Statements.insert.updateMany(
      e.toMap.toList.map { case (i, c) => toRow(k, s, i, c) }
    )

  def streamInsert(k: EphemerisKey, s: Site): Pipe[ConnectionIO, Ephemeris.Element, Unit] =
    _.map { case (i, c) => toRow(k, s, i, c) } // Stream[M, EphemerisRow]
     .chunkN(4096)                             // Stream[M, Chunk[EphemerisRow]]
     .flatMap { rows =>
       Stream.eval(Statements.insert.updateMany(rows.toVector).void)
     }

  def delete(k: EphemerisKey, s: Site): ConnectionIO[Int] =
    Statements.delete(k, s).run

  def update(k: EphemerisKey, s: Site, e: Ephemeris): ConnectionIO[Unit] =
    (delete(k, s) *> insert(k, s, e)).void

  def streamUpdate(k: EphemerisKey, s: Site): Pipe[ConnectionIO, Ephemeris.Element, Unit] =
    elems => streamInsert(k, s).apply(Stream.eval_(delete(k, s)) ++ elems)

  /** Selects all ephemeris elements associated with the given key and site into
    * an Ephemeris object.
    */
  def selectAll(k: EphemerisKey, s: Site): ConnectionIO[Ephemeris] =
    runSelect(Statements.select(k, s))

  /** Selects all ephemeris elements that fall between start and end (inclusive)
    * into an Ephemeris object.
    *
    * @param k     ephemeris key to match
    * @param s     site to match
    * @param start start time (inclusive)
    * @param end   end time (inclusive)
    *
    * @return Ephemeris object with just the matching elements
    */
  def selectRange(k: EphemerisKey, s: Site, start: Timestamp, end: Timestamp): ConnectionIO[Ephemeris] =
    runSelect(Statements.selectRange(k, s, start, end))

  private def runSelect(q: Query0[Ephemeris.Element]): ConnectionIO[Ephemeris] =
    q.to[List].map(Ephemeris.fromFoldable[List])

  def streamAll(k: EphemerisKey, s: Site): Stream[ConnectionIO, Ephemeris.Element] =
    Statements.select(k, s).stream

  def streamRange(k: EphemerisKey, s: Site, start: Timestamp, end: Timestamp): Stream[ConnectionIO, Ephemeris.Element] =
    Statements.selectRange(k, s, start, end).stream

  /** Selects the time just before (or at) start and just after (or at) end for
    * which ephemeris elements are defined for the given key and site.
    */
  def bracketRange(k: EphemerisKey, s: Site, start: Timestamp, end: Timestamp): ConnectionIO[(Timestamp, Timestamp)] =
    for {
      startʹ <- Statements.selectTimeLE(k, s, start).unique
      endʹ   <- Statements.selectTimeGE(k, s, end  ).unique
    } yield (startʹ.getOrElse(start), endʹ.getOrElse(end))

  /** Selects the min and max times for which an ephemeris is available, if any.
    */
  def selectTimes(k: EphemerisKey, s: Site): ConnectionIO[Option[(Timestamp, Timestamp)]] =
    Statements.selectTimes(k, s).unique

  /** Create the next UserSupplied ephemeris key value. */
  val nextUserSuppliedKey: ConnectionIO[EphemerisKey.UserSupplied] =
    Statements.selectNextUserSuppliedKey.unique

  def insertMeta(k: EphemerisKey, s: Site, m: EphemerisMeta): ConnectionIO[Int] =
    Statements.insertMeta(k, s, m).run

  def updateMeta(k: EphemerisKey, s: Site, m: EphemerisMeta): ConnectionIO[Int] =
    Statements.updateMeta(k, s, m).run

  def deleteMeta(k: EphemerisKey, s: Site): ConnectionIO[Int] =
    Statements.deleteMeta(k, s).run

  def selectMeta(k: EphemerisKey, s: Site): ConnectionIO[Option[EphemerisMeta]] =
    Statements.selectMeta(k, s).option

  def selectKeys(s: Site): ConnectionIO[Set[EphemerisKey]] =
    Statements.selectKeys(s).to[Set]

  object Statements {

    type EphemerisRow = (EphemerisKey, Site, Timestamp, Coordinates, String, String, Offset)

    val insert: Update[EphemerisRow] =
      Update[EphemerisRow](
        s"""
          INSERT INTO ephemeris (key_type,
                                 key,
                                 site,
                                 timestamp,
                                 ra,
                                 dec,
                                 ra_str,
                                 dec_str,
                                 delta_ra,
                                 delta_dec)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """)

    def delete(k: EphemerisKey, s: Site): Update0 =
      sql"""
        DELETE FROM ephemeris
              WHERE key_type = ${k.keyType} AND key = ${k.des} AND site = $s
      """.update

    private def selectFragment(k: EphemerisKey, s: Site): Fragment =
      fr"""
         SELECT timestamp,
                ra,
                dec,
                delta_ra,
                delta_dec
           FROM ephemeris
          WHERE key_type = ${k.keyType} AND key = ${k.des} AND site = $s
      """

    def select(k: EphemerisKey, s: Site): Query0[Ephemeris.Element] =
      selectFragment(k, s).query[Ephemeris.Element]

    def selectRange(k: EphemerisKey, s: Site, start: Timestamp, end: Timestamp): Query0[Ephemeris.Element] =
      (selectFragment(k, s) ++ fr"""AND timestamp >= $start AND timestamp <= $end""")
        .query[Ephemeris.Element]

    def selectTimeLE(k: EphemerisKey, s: Site, start: Timestamp): Query0[Option[Timestamp]] =
      sql"""
        SELECT max(timestamp)
          FROM ephemeris
         WHERE key_type = ${k.keyType} AND key = ${k.des} AND site = $s AND timestamp <= $start
      """.query[Option[Timestamp]]

    def selectTimeGE(k: EphemerisKey, s: Site, end: Timestamp): Query0[Option[Timestamp]] =
      sql"""
        SELECT min(timestamp)
          FROM ephemeris
         WHERE key_type = ${k.keyType} AND key = ${k.des} AND site = $s AND timestamp >= $end
      """.query[Option[Timestamp]]

    def selectTimes(k: EphemerisKey, s: Site): Query0[Option[(Timestamp, Timestamp)]] =
      sql"""
          SELECT min(timestamp),
                 max(timestamp)
            FROM ephemeris
           WHERE key_type = ${k.keyType} AND key = ${k.des} AND site = $s
      """.query[Option[(Timestamp, Timestamp)]]

    val selectNextUserSuppliedKey: Query0[EphemerisKey.UserSupplied] =
      sql"""
        SELECT nextval('user_ephemeris_id')
      """.query[Long].map(id => EphemerisKey.UserSupplied(id.toInt))

    def insertMeta(k: EphemerisKey, s: Site, m: EphemerisMeta): Update0 =
      (fr"""
        INSERT INTO ephemeris_meta (
            key_type,
            key,
            site,
            last_update,
            last_update_check,
            horizons_soln_ref
        ) VALUES""" ++ values((k, s, m))).update

    def updateMeta(k: EphemerisKey, s: Site, m: EphemerisMeta): Update0 =
      (fr"""
        UPDATE ephemeris_meta
           SET (last_update,
                last_update_check,
                horizons_soln_ref
        ) =""" ++ values(m) ++
      fr"WHERE key_type = ${k.keyType} AND key = ${k.des} AND site = $s").update

    def deleteMeta(k: EphemerisKey, s: Site): Update0 =
      sql"""
         DELETE FROM ephemeris_meta
               WHERE key_type = ${k.keyType} AND key = ${k.des} AND site = $s
       """.update

    def selectMeta(k: EphemerisKey, s: Site): Query0[EphemerisMeta] =
      sql"""
        SELECT last_update,
               last_update_check,
               horizons_soln_ref
          FROM ephemeris_meta
         WHERE key_type = ${k.keyType} AND key = ${k.des} AND site = $s
      """.query[EphemerisMeta]

    def selectKeys(s: Site): Query0[EphemerisKey] =
      sql"""
        SELECT key_type,
               key
          FROM ephemeris_meta
         WHERE site = ${s}
      """.query[EphemerisKey]
  }
}
