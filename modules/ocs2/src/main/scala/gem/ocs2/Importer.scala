// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2

import doobie.imports._

import gem.dao._

import gem.{Dataset, Log, Observation, Program, Step, User}
import gem.config.{ StaticConfig, DynamicConfig }

import scalaz.Scalaz._
import scalaz.concurrent.Task

/** Support for writing programs and observations to the database.
  */
object Importer extends DoobieClient {

  def writeObservation(o: Observation[StaticConfig, Step[DynamicConfig]], ds: List[Dataset]): (User[_], Log[ConnectionIO]) => ConnectionIO[Unit] = {

    val rmObservation: ConnectionIO[Unit] =
      sql"DELETE FROM observation WHERE observation_id = ${o.id}".update.run.void

    val lookupStepIds: ConnectionIO[List[Int]] =
      sql"SELECT step_id FROM step WHERE observation_id = ${o.id} ORDER BY location".query[Int].list

    def datasetTuples(sids: List[Int]): List[(Int, Dataset)] = {
      val sidMap = sids.zipWithIndex.map(_.swap).toMap
      ds.flatMap { d => sidMap.get(d.label.index - 1).map(_ -> d).toList }
    }

    val writeDatasets: ConnectionIO[Unit] =
      for {
        sids <- lookupStepIds
        _    <- datasetTuples(sids).traverseU { case (sid, d) => DatasetDao.insert(sid, d) }.void
      } yield ()

    (u: User[_], l: Log[ConnectionIO]) =>
      for {
        _ <- ignoreUniqueViolation(ProgramDao.insertFlat(Program[Nothing](o.id.pid, "", Nil)).as(1))
        _ <- l.log(u, s"remove observation ${o.id}"   )(rmObservation           )
        _ <- l.log(u, s"insert new version of ${o.id}")(ObservationDao.insert(o))
        _ <- l.log(u, s"write datasets for ${o.id}"   )(writeDatasets           )
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
        _ <- p.observations.traverseU(o => writeObservation(o, dsMap(o.id))(u, l))
      } yield ()
  }

  def doImport(write: (User[_], Log[ConnectionIO]) => ConnectionIO[Unit]): Task[Unit] =
    for {
      u <- UserDao.selectRoot.transact(lxa)
      l <- Log.newLog[ConnectionIO]("importer", lxa).transact(lxa)
      _ <- Task.delay(configureLogging)
      _ <- write(u, l).transact(lxa)
      _ <- l.shutdown(5 * 1000).transact(lxa) // if we're not done soon something is wrong
    } yield ()

  def importObservation(o: Observation[StaticConfig, Step[DynamicConfig]], ds: List[Dataset]): Task[Unit] =
    doImport(writeObservation(o, ds))

  def importProgram(p: Program[Observation[StaticConfig, Step[DynamicConfig]]], ds: List[Dataset]): Task[Unit] =
    doImport(writeProgram(p, ds))
}
