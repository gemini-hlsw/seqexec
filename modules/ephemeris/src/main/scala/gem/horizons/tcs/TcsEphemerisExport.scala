// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons.tcs

import gem.EphemerisKey
import gem.dao.EphemerisDao
import gem.enum.Site
import gem.util.Timestamp

import cats.effect._
import cats.implicits._
import doobie._
import doobie.implicits._

import fs2.Stream
import fs2.text
import fs2.io.file

import java.nio.file.Path
import java.nio.file.StandardOpenOption.{ CREATE, TRUNCATE_EXISTING }

/** Provides support for exporting ephemeris data to files that may be read by
  * the TCS.
  *
  * @param xa transactor to use for working with the database
  */
final class TcsEphemerisExport[M[_]: Sync: ContextShift](xa: Transactor[M]) {
  import TcsEphemerisExport.RowLimit

  /** Produces the name of the corresponding .eph file. */
  def fileName(k: EphemerisKey): String =
    s"${EphemerisKey.fromString.reverseGet(k)}.eph"

  /** Resolves the ephemeris file corresponding to the given key. */
  def resolve(key: EphemerisKey, dir: Path): Path =
    dir.resolve(fileName(key))

  /** Exports up to `RowLimit` lines of ephemeris data associated with the given
    * key, site, and time range.
    *
    * Unless the start or end exactly matches an ephemeris element, then an
    * entry before start, and/or after end will also be included. This ensures
    * that the time range is fully covered and a position at any point in the
    * range can be inferred from the ephemeris.
    *
    * @param path  directory to which the ephemeris data should be written
    * @param key   key corresponding to the object to export
    * @param site  site for which the data is relevant
    * @param start start time for ephemeris data; if there is an element at
    *              exactly this time it will be the first element (otherwise,
    *              the element immediately preceeding this time is included)
    * @param end   end time for ephemeris data; if there is an element at
    *              exactly this time it will be the last element (otherwise,
    *              the element immediately following this time is included)
    */
  def exportOne(key: EphemerisKey, site: Site, start: Timestamp, end: Timestamp, dir: Path)(blocker: Blocker): M[Unit] = {
    import EphemerisDao.{ bracketRange, streamRange }

    Stream.eval(bracketRange(key, site, start, end))
      .flatMap { case (s, e) => streamRange(key, site, s, e) }
      .transact(xa)
      .take(RowLimit.toLong)
      .through(TcsFormat.ephemeris)
      .intersperse("\n")
      .append(Stream.emit("\n"))
      .through(text.utf8Encode)
      .through(file.writeAll(resolve(key, dir), blocker, List(CREATE, TRUNCATE_EXISTING)))
      .compile
      .drain
  }

  /** Exports all ephemerides for the given site for the given time period. File
    * names are invented based upon the corresponding ephemeris key.  See
    * `EphemerisKey.format`.
    *
    * @param dir   directory where the ephemerides will be written
    * @param site  site to which the ephemerides correspond
    * @param start start time for the ephemeris data, inclusive
    * @param end   end time for the ephemeirs day, exclusive
    */
  def exportAll(site: Site, start: Timestamp, end: Timestamp, dir: Path)(blocker: Blocker): M[Unit] =
    for {
      ks <- EphemerisDao.selectKeys(site).transact(xa)
      _  <- ks.toList.traverse_(k => exportOne(k, site, start, end, resolve(k, dir))(blocker))
    } yield ()
}

object TcsEphemerisExport {

  /** Max ephemeris elements supported by the TCS. */
  val RowLimit: Int =
    1440
}
