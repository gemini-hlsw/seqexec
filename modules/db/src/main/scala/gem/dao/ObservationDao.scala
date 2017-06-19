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

package gem
package dao

import gem.config.{DynamicConfig, StaticConfig}
import gem.enum.Instrument

import doobie.imports._

import scalaz._, Scalaz._

object ObservationDao {

  def insert(o: Observation[StaticConfig, Step[DynamicConfig]]): ConnectionIO[Int] =
    for {
      id <- StaticConfigDao.insert(o.staticConfig)
      _  <- Statements.insert(o, id).run
      _  <- o.steps.zipWithIndex.traverseU { case (s, i) =>
              StepDao.insert(o.id, Location.unsafeMiddle((i + 1) * 100), s)
            }.void
    } yield id

  /** Select all the observation ids associated with the given program. */
  def selectIds(pid: Program.Id): ConnectionIO[List[Observation.Id]] =
    Statements.selectIds(pid).list

  def selectFlat(id: Observation.Id): ConnectionIO[Observation[Instrument, Nothing]] =
    Statements.selectFlat(id).unique.map(_._1)

  def selectStatic(id: Observation.Id): ConnectionIO[Observation[StaticConfig, Nothing]] =
    for {
      obs <- selectFlat(id)
      tup <- Statements.selectStaticId(id).unique
      sc  <- StaticConfigDao.select(tup._1, tup._2)
    } yield obs.leftMap(_ => sc)

  def select(id: Observation.Id): ConnectionIO[Observation[StaticConfig, Step[DynamicConfig]]] =
    for {
      on <- selectStatic(id)
      ss <- StepDao.selectAll(id)
    } yield on.copy(steps = ss.values)

  def selectAllFlat(pid: Program.Id): ConnectionIO[List[Observation[Instrument, Nothing]]] =
    Statements.selectAllFlat(pid).list.map(_.map(_._1))

  def selectAllStatic(pid: Program.Id): ConnectionIO[List[Observation[StaticConfig, Nothing]]] =
    for {
      ids <- selectIds(pid)
      oss <- ids.traverseU(selectStatic)
    } yield oss

  def selectAll(pid: Program.Id): ConnectionIO[List[Observation[StaticConfig, Step[DynamicConfig]]]] =
    for {
      ids <- selectIds(pid)
      oss <- ids.traverseU(select)
    } yield oss

  object Statements {

    // ObservationIndex has a DISTINCT type due to its check constraint so we need a fine-grained mapping
    // here to satisfy the query checker.
    private case class ObservationIndex(toInt: Int)
    private object ObservationIndex {
      implicit val ObservationIndexMeta: Meta[ObservationIndex] =
        Distinct.integer("id_index").xmap(ObservationIndex(_), _.toInt)
    }

    def insert(o: Observation[StaticConfig, _], staticId: Int): Update0 =
      sql"""
        INSERT INTO observation (observation_id,
                                program_id,
                                observation_index,
                                title,
                                static_id,
                                instrument)
              VALUES (${o.id},
                      ${o.id.pid},
                      ${ObservationIndex(o.id.index)},
                      ${o.title},
                      $staticId,
                      ${o.staticConfig.instrument : Instrument})
      """.update

    def selectIds(pid: Program.Id): Query0[Observation.Id] =
      sql"""
        SELECT observation_id
          FROM observation
         WHERE program_id = $pid
      """.query[Observation.Id]

    def selectStaticId(id: Observation.Id): Query0[(Instrument, Int)] =
      sql"""
        SELECT instrument, static_id
          FROM observation
         WHERE observation_id = $id
      """.query[(Instrument, Int)]

    def selectFlat(id: Observation.Id): Query0[(Observation[Instrument, Nothing], Int)] =
      sql"""
        SELECT title, instrument, static_id
          FROM observation
         WHERE observation_id = ${id}
      """.query[(String, Instrument, Int)]
         .map { case (t, i, s) =>
           (Observation(id, t, i, Nil), s)
         }

    def selectAllFlat(pid: Program.Id): Query0[(Observation[Instrument, Nothing], Int)] =
      sql"""
        SELECT observation_index, title, instrument, static_id
          FROM observation
         WHERE program_id = ${pid}
      ORDER BY observation_index
      """.query[(Short, String, Instrument, Int)]
         .map { case (n, t, i, s) =>
           (Observation(Observation.Id(pid, n.toInt), t, i, Nil), s)
         }

  }
}
