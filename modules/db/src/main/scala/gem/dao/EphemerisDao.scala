// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.enum.EphemerisKeyType
import gem.math._

import cats.Monad
import cats.implicits._
import doobie._, doobie.implicits._
import fs2.Stream

import java.time.Instant

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

  def selectAll(k: EphemerisKey): ConnectionIO[Ephemeris] =
    runSelect(Statements.select(k))

  def selectBetween(k: EphemerisKey, start: Instant, end: Instant): ConnectionIO[Ephemeris] =
    runSelect(Statements.selectBetween(k, start, end))

  private def runSelect(q: Query0[Ephemeris.Element]): ConnectionIO[Ephemeris] =
    q.list.map(Ephemeris.fromFoldable[List])

  def streamAll(k: EphemerisKey): Stream[ConnectionIO, Ephemeris.Element] =
    Statements.select(k).stream

  def streamBetween(k: EphemerisKey, start: Instant, end: Instant): Stream[ConnectionIO, Ephemeris.Element] =
    Statements.selectBetween(k, start, end).stream

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

    type EphemerisRow = (EphemerisKey, Instant, Coordinates, String, String)

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

    def selectBetween(k: EphemerisKey, s: Instant, e: Instant): Query0[Ephemeris.Element] =
      (selectFragment(k) ++ fr"""AND timestamp >= $s AND timestamp < $e""")
        .query[Ephemeris.Element]

  }
}
