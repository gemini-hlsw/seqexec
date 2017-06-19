/*
 * Copyright (c) 2017, Association of Universities for Research in Astronomy, Inc. (AURA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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
      ds.flatMap { d => sidMap.get(d.label.index - 1).map(_ -> d) }
    }

    val writeDatasets: ConnectionIO[Unit] =
      for {
        sids <- lookupStepIds
        _    <- datasetTuples(sids).traverseU { case (sid, d) => DatasetDao.insert(sid, d) }.void
      } yield ()

    (u: User[_], l: Log[ConnectionIO]) =>
      for {
        _ <- ignoreUniqueViolation(ProgramDao.insert(Program[Nothing](o.id.pid, "", Nil)))
        _ <- l.log(u, s"remove observation ${o.id}"   )(rmObservation           )
        _ <- l.log(u, s"insert new version of ${o.id}")(ObservationDao.insert(o))
        _ <- l.log(u, s"write datasets for ${o.id}"   )(writeDatasets           )
      } yield ()
  }

  def writeProgram(p: Program[Observation[StaticConfig, Step[DynamicConfig]]], ds: List[Dataset]): (User[_], Log[ConnectionIO]) => ConnectionIO[Unit] = {
    val rmProgram: ConnectionIO[Unit] =
      sql"DELETE FROM program WHERE program_id = ${p.id}".update.run.void

    val dsMap = ds.groupBy(_.label.oid).withDefaultValue(List.empty[Dataset])

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
