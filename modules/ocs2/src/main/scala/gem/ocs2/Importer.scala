// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2

import gem.dao._
import gem.{ Dataset, Log, Observation, Program, Step, User }
import gem.config.{ StaticConfig, DynamicConfig }

import cats.effect.IO
import cats.implicits._
import doobie._, doobie.implicits._

import scala.collection.immutable.TreeMap


/** Support for writing programs and observations to the database.
  */
object Importer extends DoobieClient {

  object datasets {
    def lookupStepIds(oid: Observation.Id): ConnectionIO[List[Int]] =
      sql"SELECT step_id FROM step WHERE program_id = ${oid.pid} AND observation_index = ${oid.index} ORDER BY location".query[Int].list

    def tuples(sids: List[Int], ds: List[Dataset]): List[(Int, Dataset)] = {
      val sidMap = sids.zipWithIndex.map(_.swap).toMap
      ds.flatMap { d => sidMap.get(d.label.index - 1).map(_ -> d).toList }
    }

    def write(oid: Observation.Id, ds: List[Dataset]): ConnectionIO[Unit] =
      for {
        sids <- lookupStepIds(oid)
        _    <- tuples(sids, ds).traverse_ { case (sid, d) => DatasetDao.insert(sid, d) }
      } yield ()
  }

  def writeObservation(oid: Observation.Id, o: Observation[StaticConfig, Step[DynamicConfig]], ds: List[Dataset]): (User[_], Log[ConnectionIO]) => ConnectionIO[Unit] = {

    val rmObservation: ConnectionIO[Unit] =
      sql"DELETE FROM observation WHERE program_id = ${oid.pid} AND observation_index = ${oid.index}".update.run.void

    (u: User[_], l: Log[ConnectionIO]) =>
      for {
        _ <- ignoreUniqueViolation(ProgramDao.insertFlat(Program[Nothing](oid.pid, "", TreeMap.empty)).as(1))
        _ <- l.log(u, s"remove observation $oid"   )(rmObservation                )
        _ <- l.log(u, s"insert new version of $oid")(ObservationDao.insert(oid, o))
        _ <- l.log(u, s"write datasets for $oid"   )(datasets.write(oid, ds)      )
      } yield ()
  }

  def writeProgram(p: Program[Observation[StaticConfig, Step[DynamicConfig]]], ds: List[Dataset]): (User[_], Log[ConnectionIO]) => ConnectionIO[Unit] = {
    val rmProgram: ConnectionIO[Unit] =
      sql"DELETE FROM program WHERE program_id = ${p.id}".update.run.void

    val dsMap = ds.groupBy(_.label.observationId).withDefaultValue(List.empty[Dataset])

    (u: User[_], l: Log[ConnectionIO]) =>
      for {
        _ <- l.log(u, s"remove program ${p.id}"       )(rmProgram           )
        _ <- l.log(u, s"insert new version of ${p.id}")(ProgramDao.insert(p))
        _ <- p.observations.toList.traverse_ { case (i,o) =>
               val oid = Observation.Id(p.id, i)
               l.log(u, s"write datasets for $oid")(datasets.write(oid, dsMap(oid)))
             }
      } yield ()
  }

  def doImport(write: (User[_], Log[ConnectionIO]) => ConnectionIO[Unit]): IO[Unit] =
    for {
      u <- UserDao.selectRootUser.transact(lxa)
      l <- Log.newLog[ConnectionIO]("importer", lxa).transact(lxa)
      _ <- IO(configureLogging)
      _ <- write(u, l).transact(lxa)
      _ <- l.shutdown(5 * 1000).transact(lxa) // if we're not done soon something is wrong
    } yield ()

  def importObservation(oid: Observation.Id, o: Observation[StaticConfig, Step[DynamicConfig]], ds: List[Dataset]): IO[Unit] =
    doImport(writeObservation(oid, o, ds))

  def importProgram(p: Program[Observation[StaticConfig, Step[DynamicConfig]]], ds: List[Dataset]): IO[Unit] =
    doImport(writeProgram(p, ds))
}
