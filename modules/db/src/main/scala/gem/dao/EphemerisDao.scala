// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.dao.composite._
import gem.dao.meta._
import gem.enum.Site
import gem.math._
import gem.util.InstantMicros

import cats.Monad
import cats.implicits._
import doobie._, doobie.implicits._
import fs2.Stream

object EphemerisDao {
  import CoordinatesComposite._
  import EnumeratedMeta._
  import EphemerisKeyComposite._
  import TimeMeta._

  def insert(k: EphemerisKey, s: Site, e: Ephemeris): ConnectionIO[Int] =
    Statements.insert.updateMany(
      e.toMap.toList.map { case (i, c) => (k, s, i, c, c.ra.format, c.dec.format) }
    )

  def streamInsert[M[_]: Monad](k: EphemerisKey, s: Site, sm: Stream[M, Ephemeris.Element], xa: Transactor[M]): Stream[M, Int] =
    sm.map { case (i, c) => (k, s, i, c, c.ra.format, c.dec.format) } // Stream[M, EphemerisRow]
      .segmentN(4096)                                                 // Stream[M, Segment[EphemerisRow, Unit]]
      .flatMap { rows =>
        Stream.eval(Statements.insert.updateMany(rows.toVector).transact(xa))
      }

  def delete(k: EphemerisKey, s: Site): ConnectionIO[Int] =
    Statements.delete(k, s).run

  def update(k: EphemerisKey, s: Site, e: Ephemeris): ConnectionIO[Unit] =
    (delete(k, s) *> insert(k, s, e)).void

  /** Selects all ephemeris elements associated with the given key and site into
    * an Ephemeris object.
    */
  def selectAll(k: EphemerisKey, s: Site): ConnectionIO[Ephemeris] =
    runSelect(Statements.select(k, s))

  /** Selects all ephemeris elements that fall between start (inclusive) and end
    * (exclusive) into an Ephemeris object.
    *
    * @param k     ephemeris key to match
    * @param s     site to match
    * @param start start time (inclusive)
    * @param end   end time (exclusive
    *
    * @return Ephemeris object with just the matching elements
    */
  def selectRange(k: EphemerisKey, s: Site, start: InstantMicros, end: InstantMicros): ConnectionIO[Ephemeris] =
    runSelect(Statements.selectRange(k, s, start, end))

  private def runSelect(q: Query0[Ephemeris.Element]): ConnectionIO[Ephemeris] =
    q.list.map(Ephemeris.fromFoldable[List])

  def streamAll(k: EphemerisKey, s: Site): Stream[ConnectionIO, Ephemeris.Element] =
    Statements.select(k, s).stream

  def streamRange(k: EphemerisKey, s: Site, start: InstantMicros, end: InstantMicros): Stream[ConnectionIO, Ephemeris.Element] =
    Statements.selectRange(k, s, start, end).stream

  /** Create the next UserSupplied ephemeris key value. */
  val nextUserSuppliedKey: ConnectionIO[EphemerisKey.UserSupplied] =
    Statements.selectNextUserSuppliedKey.unique

  object Statements {

    type EphemerisRow = (EphemerisKey, Site, InstantMicros, Coordinates, String, String)

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
                                 dec_str)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?)
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
                dec
           FROM ephemeris
          WHERE key_type = ${k.keyType} AND key = ${k.des} AND site = $s
      """

    def select(k: EphemerisKey, s: Site): Query0[Ephemeris.Element] =
      selectFragment(k, s).query[Ephemeris.Element]

    def selectRange(k: EphemerisKey, s: Site, start: InstantMicros, end: InstantMicros): Query0[Ephemeris.Element] =
      (selectFragment(k, s) ++ fr"""AND timestamp >= $start AND timestamp < $end""")
        .query[Ephemeris.Element]

    val selectNextUserSuppliedKey: Query0[EphemerisKey.UserSupplied] =
      sql"""
        SELECT nextval('user_ephemeris_id')
      """.query[Long].map(id => EphemerisKey.UserSupplied(id.toInt))

  }
}
