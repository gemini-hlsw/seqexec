// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package ocs2

import gem.dao._

import cats.implicits._
import doobie._, doobie.implicits._

import scala.collection.immutable.TreeMap


/** Support for writing programs and observations to the database.
  */
object Importer extends DoobieClient {

  object datasets {
    def lookupStepIds(oid: Observation.Id): ConnectionIO[List[Int]] =
      sql"SELECT step_id FROM step WHERE program_id = ${oid.pid} AND observation_index = ${oid.index} ORDER BY location".query[Int].to[List]

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

  def importObservation(oid: Observation.Id, o: Observation, ds: List[Dataset]): ConnectionIO[Unit] = {

    val rmObservation: ConnectionIO[Unit] =
      sql"DELETE FROM observation WHERE program_id = ${oid.pid} AND observation_index = ${oid.index}".update.run.void

    for {
      _ <- ignoreUniqueViolation(ProgramDao.insertFlat(Program(oid.pid, "", TreeMap.empty)).as(1))
      _ <- rmObservation
      _ <- ObservationDao.insert(oid, o)
      _ <- datasets.write(oid, ds)
    } yield ()
  }

  def importProgram(p: Program, ds: List[Dataset]): ConnectionIO[Unit] = {

    val rmProgram: ConnectionIO[Unit] =
      sql"DELETE FROM program WHERE program_id = ${p.id}".update.run.void

    val dsMap = ds.groupBy(_.label.observationId).withDefaultValue(List.empty[Dataset])

    for {
      _ <- rmProgram
      _ <- ProgramDao.insert(p)
      _ <- p.observations.toList.traverse_ { case (i, _) =>
             val oid = Observation.Id(p.id, i)
             datasets.write(oid, dsMap(oid))
           }
    } yield ()
  }

}
