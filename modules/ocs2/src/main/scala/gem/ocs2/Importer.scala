// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2

import gem.dao._
import gem.{ Dataset, Log, Observation, Program, Step, Target, User }
import gem.config.{ StaticConfig, DynamicConfig }

import cats.effect.IO
import cats.implicits._
import doobie._, doobie.implicits._

import scala.collection.immutable.TreeMap


/** Support for writing programs and observations to the database.
  */
object Importer extends DoobieClient {

  def writeObservation(oid: Observation.Id, o: Observation[StaticConfig, Step[DynamicConfig]], ds: List[Dataset]): (User[_], Log[ConnectionIO]) => ConnectionIO[Unit] = {

    val rmObservation: ConnectionIO[Unit] =
      sql"DELETE FROM observation WHERE observation_id = ${oid}".update.run.void

    val lookupStepIds: ConnectionIO[List[Int]] =
      sql"SELECT step_id FROM step WHERE observation_id = ${oid} ORDER BY location".query[Int].list

    def datasetTuples(sids: List[Int]): List[(Int, Dataset)] = {
      val sidMap = sids.zipWithIndex.map(_.swap).toMap
      ds.flatMap { d => sidMap.get(d.label.index - 1).map(_ -> d).toList }
    }

    val writeDatasets: ConnectionIO[Unit] =
      for {
        sids <- lookupStepIds
        _    <- datasetTuples(sids).traverse { case (sid, d) => DatasetDao.insert(sid, d) }.void
      } yield ()

    (u: User[_], l: Log[ConnectionIO]) =>
      for {
        _ <- ignoreUniqueViolation(ProgramDao.insertFlat(Program[Nothing](oid.pid, "", TreeMap.empty)).as(1))
        _ <- l.log(u, s"remove observation ${oid}"   )(rmObservation                )
        _ <- l.log(u, s"insert new version of ${oid}")(ObservationDao.insert(oid, o))
        _ <- l.log(u, s"write datasets for ${oid}"   )(writeDatasets                )
      } yield ()
  }

  def writeProgram(p: Program[Observation[StaticConfig, Step[DynamicConfig]]], ds: List[Dataset], ts: List[Target]): (User[_], Log[ConnectionIO]) => ConnectionIO[Unit] = {
    val rmProgram: ConnectionIO[Unit] =
      sql"DELETE FROM program WHERE program_id = ${p.id}".update.run.void

    val dsMap = ds.groupBy(_.label.observationId).withDefaultValue(List.empty[Dataset])

    (u: User[_], l: Log[ConnectionIO]) =>
      for {
        _ <- l.log(u, s"remove program ${p.id}"       )(rmProgram           )
        _ <- l.log(u, s"insert new version of ${p.id}")(ProgramDao.insert(p))
        _ <- p.observations.toList.traverse { case (i,o) =>
               val oid = Observation.Id(p.id, i)
               writeObservation(oid, o, dsMap(oid))(u, l)
             }
        _ <- l.log(u, s"insert targets from ${p.id}") {
               ts.traverse_(TargetDao.insert)
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

  def importProgram(p: Program[Observation[StaticConfig, Step[DynamicConfig]]], ds: List[Dataset], ts: List[Target]): IO[Unit] =
    doImport(writeProgram(p, ds, ts))
}
