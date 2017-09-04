// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.enum.EphemerisKeyType
import gem.math._
import gem.util.InstantMicros

import cats.Monad
import cats.implicits._
import doobie._, doobie.implicits._
import fs2.Stream

object EphemerisDao {

  def insert(k: EphemerisKey, e: Ephemeris): ConnectionIO[Int] =
    Statements.insert.updateMany(
      e.toMap.toList.map { case (i, c) => (k, i, c, c.ra.format, c.dec.format) }
    )

  def streamInsert[M[_]: Monad](k: EphemerisKey, s: Stream[M, Ephemeris.Element], xa: Transactor[M]): Stream[M, Int] =
    Stream.constant(k)                                                  // Stream[Pure, EphemerisKey]
      .zip(s)                                                           // Stream[M, (EphemerisKey, Ephemeris.Element)]
      .map { case (k, (i, c)) => (k, i, c, c.ra.format, c.dec.format) } // Stream[M, EphemerisRow]
      .segmentN(4096)                                                   // Stream[M, Segment[EphemerisRow, Unit]]
      .flatMap { rows =>
        Stream.eval(Statements.insert.updateMany(rows.toVector).transact(xa))
      }

  def delete(k: EphemerisKey): ConnectionIO[Int] =
    Statements.delete(k).run

  def update(k: EphemerisKey, e: Ephemeris): ConnectionIO[Unit] =
    (delete(k) *> insert(k, e)).void

  /** Selects all ephemeris elements associated with the given key into an
    * Ephemeris object.
    */
  def selectAll(k: EphemerisKey): ConnectionIO[Ephemeris] =
    runSelect(Statements.select(k))

  /** Selects all ephemeris elements that fall between start (inclusive) and end
    * (exclusive) into an Ephemeris object.
    *
    * @param k     key to match
    * @param start start time (inclusive)
    * @param end   end time (exclusive
    *
    * @return Ephemeris object with just the matching elements
    */
  def selectRange(k: EphemerisKey, start: InstantMicros, end: InstantMicros): ConnectionIO[Ephemeris] =
    runSelect(Statements.selectRange(k, start, end))

  private def runSelect(q: Query0[Ephemeris.Element]): ConnectionIO[Ephemeris] =
    q.list.map(Ephemeris.fromFoldable[List])

  def streamAll(k: EphemerisKey): Stream[ConnectionIO, Ephemeris.Element] =
    Statements.select(k).stream

  def streamRange(k: EphemerisKey, start: InstantMicros, end: InstantMicros): Stream[ConnectionIO, Ephemeris.Element] =
    Statements.selectRange(k, start, end).stream

  object Statements {

    // Describe how to turn an EphemerisKey into a (EphemerisKeyType, String)
    implicit val CompositeEphemerisKey: Composite[EphemerisKey] =
      Composite[(EphemerisKeyType, String)].imap(
        (t: (EphemerisKeyType, String)) => EphemerisKey.unsafeFromTypeAndDes(t._1, t._2))(
        (k: EphemerisKey)               => (k.keyType, k.des)
      )

    // Describe how to map Coordinates into (Long, Long)
    implicit val CompositeCoordinates: Composite[Coordinates] =
      Composite[(Long, Long)].imap(
        (t: (Long, Long)) =>
          Coordinates(RightAscension(HourAngle.fromMicroseconds(t._1)),
                      Declination.unsafeFromAngle(Angle.fromMicroarcseconds(t._2))))(
        (c: Coordinates)                  =>
          (c.ra.toHourAngle.toMicroseconds, c.dec.toAngle.toMicroarcseconds)
      )

    type EphemerisRow = (EphemerisKey, InstantMicros, Coordinates, String, String)

    val insert: Update[EphemerisRow] =
      Update[EphemerisRow](
        s"""
          INSERT INTO ephemeris (key_type,
                                 key,
                                 timestamp,
                                 ra,
                                 dec,
                                 ra_str,
                                 dec_str)
               VALUES (?, ?, ?, ?, ?, ?, ?)
        """)

    def delete(k: EphemerisKey): Update0 =
      sql"""
        DELETE FROM ephemeris
              WHERE key_type = ${k.keyType} AND key = ${k.des}
      """.update

    private def selectFragment(k: EphemerisKey): Fragment =
      fr"""
         SELECT timestamp,
                ra,
                dec
           FROM ephemeris
          WHERE key_type = ${k.keyType} AND key = ${k.des}
      """

    def select(k: EphemerisKey): Query0[Ephemeris.Element] =
      selectFragment(k).query[Ephemeris.Element]

    def selectRange(k: EphemerisKey, s: InstantMicros, e: InstantMicros): Query0[Ephemeris.Element] =
      (selectFragment(k) ++ fr"""AND timestamp >= $s AND timestamp < $e""")
        .query[Ephemeris.Element]

  }
}
