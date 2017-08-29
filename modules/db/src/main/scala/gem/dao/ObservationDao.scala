// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import cats.implicits._
import doobie._, doobie.implicits._
import gem.config.{DynamicConfig, StaticConfig}
import gem.enum.Instrument
import gem.util.Location

object ObservationDao {

  /**
   * Construct a program to insert a fully-populated Observation. This program will raise a
   * key violation if an observation with the same id already exists.
   */
  def insert(o: Observation[StaticConfig, Step[DynamicConfig]]): ConnectionIO[Unit] =
    for {
      id <- StaticConfigDao.insert(o.staticConfig)
      _  <- Statements.insert(o, id).run
      _  <- o.steps.zipWithIndex.traverse { case (s, i) =>
              StepDao.insert(o.id, Location.unsafeMiddle((i + 1) * 100), s)
            }.void
    } yield ()

  /** Construct a program to select the specified observation, with the instrument and no steps. */
  def selectFlat(id: Observation.Id): ConnectionIO[Observation[Instrument, Nothing]] =
    Statements.selectFlat(id).unique.map(_._1)

  /** Construct a program to select the specified observation, with static connfig and no steps. */
  def selectStatic(id: Observation.Id): ConnectionIO[Observation[StaticConfig, Nothing]] =
    for {
      obs <- selectFlat(id)
      tup <- Statements.selectStaticId(id).unique
      sc  <- StaticConfigDao.select(tup._1, tup._2)
    } yield obs.leftMap(_ => sc)

  /** Construct a program to select the specified observation, with static connfig and steps. */
  def select(id: Observation.Id): ConnectionIO[Observation[StaticConfig, Step[DynamicConfig]]] =
    for {
      on <- selectStatic(id)
      ss <- StepDao.selectAll(id)
    } yield on.copy(steps = ss.values.toList)

  /** Construct a program to select the all obseravation ids for the specified science program. */
  def selectIds(pid: Program.Id): ConnectionIO[List[Observation.Id]] =
    Statements.selectIds(pid).list

  /**
   * Construct a program to select all observations for the specified science program, with the
   * instrument and no steps.
   */
  def selectAllFlat(pid: Program.Id): ConnectionIO[List[Observation[Instrument, Nothing]]] =
    Statements.selectAllFlat(pid).list.map(_.map(_._1))

  /**
   * Construct a program to select all observations for the specified science program, with the
   * static component and no steps.
   */
  def selectAllStatic(pid: Program.Id): ConnectionIO[List[Observation[StaticConfig, Nothing]]] =
    for {
      ids <- selectIds(pid)
      oss <- ids.traverse(selectStatic)
    } yield oss

  /**
   * Construct a program to select all observations for the specified science program, with the
   * static component and steps.
   */
  def selectAll(pid: Program.Id): ConnectionIO[List[Observation[StaticConfig, Step[DynamicConfig]]]] =
    for {
      ids <- selectIds(pid)
      oss <- ids.traverse(select)
    } yield oss

  object Statements {

    // ObservationIndex has a DISTINCT type due to its check constraint so we need a fine-grained mapping
    // here to satisfy the query checker.
    private final case class ObservationIndex(toInt: Int)
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
