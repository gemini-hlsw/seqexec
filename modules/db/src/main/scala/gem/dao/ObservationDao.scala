// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import cats.implicits._
import doobie._, doobie.implicits._
import gem.config.{DynamicConfig, StaticConfig}
import gem.dao.meta._
import gem.enum.Instrument
import gem.syntax.treemapcompanion._
import gem.util.Location

import scala.collection.immutable.TreeMap

object ObservationDao {
  import EnumeratedMeta._
  import ObservationIdMeta._
  import ObservationIndexMeta._
  import ProgramIdMeta._

  /**
   * Construct a program to insert a fully-populated Observation. This program will raise a
   * key violation if an observation with the same id already exists.
   */
  def insert(oid: Observation.Id, o: Observation[StaticConfig, Step[DynamicConfig]]): ConnectionIO[Unit] =
    for {
      _ <- Statements.insert(oid, o).run
      _ <- StaticConfigDao.insert(oid, o.staticConfig)
      _ <- TargetEnvironmentDao.insert(oid, o.targets)
      _ <- o.steps.zipWithIndex.traverse { case (s, i) =>
             StepDao.insert(oid, Location.unsafeMiddle((i + 1) * 100), s)
           }.void
    } yield ()

  /** Construct a program to select the specified observation, with the instrument and no steps. */
  def selectFlat(id: Observation.Id): ConnectionIO[Observation[Instrument, Nothing]] =
    for {
      o <- Statements.selectFlat(id).unique
      t <- TargetEnvironmentDao.selectObs(id)
    } yield o.copy(targets = t)

  /** Construct a program to select the specified observation, with static connfig and no steps. */
  def selectStatic(id: Observation.Id): ConnectionIO[Observation[StaticConfig, Nothing]] =
    for {
      obs <- selectFlat(id)
      sc  <- StaticConfigDao.select(id, obs.staticConfig)
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

  private def merge[I: Ordering, S, D](
    os: TreeMap[I, Observation[S, D]],
    ts: Map[I, TargetEnvironment]
  ): TreeMap[I, Observation[S, D]] =
    os.foldLeft(TreeMap.empty[I, Observation[S, D]]) { case (m, (i, o)) =>
      m.updated(i, ts.get(i).fold(o)(t => o.copy(targets = t)))
    }

  /**
   * Construct a program to select all observations for the specified science program, with the
   * instrument and no steps.
   */
  def selectAllFlat(pid: Program.Id): ConnectionIO[TreeMap[Observation.Index, Observation[Instrument, Nothing]]] =
    for {
      m  <- Statements.selectAllFlat(pid).list.map(lst => TreeMap.fromList(lst))
      ts <- TargetEnvironmentDao.selectProg(pid)
    } yield merge(m, ts)

  /**
   * Construct a program to select all observations for the specified science program, with the
   * static component and no steps.
   */
  def selectAllStatic(pid: Program.Id): ConnectionIO[TreeMap[Observation.Index, Observation[StaticConfig, Nothing]]] =
    for {
      ids <- selectIds(pid)
      oss <- ids.traverse(selectStatic)
      ts  <- TargetEnvironmentDao.selectProg(pid)
    } yield merge(TreeMap.fromList(ids.map(_.index).zip(oss)), ts)

  /**
   * Construct a program to select all observations for the specified science program, with the
   * static component and steps.
   */
  def selectAll(pid: Program.Id): ConnectionIO[TreeMap[Observation.Index, Observation[StaticConfig, Step[DynamicConfig]]]] =
    for {
      ids <- selectIds(pid)
      oss <- ids.traverse(select)
      ts  <- TargetEnvironmentDao.selectProg(pid)
    } yield merge(TreeMap.fromList(ids.map(_.index).zip(oss)), ts)

  object Statements {

    def insert(oid: Observation.Id, o: Observation[StaticConfig, _]): Update0 =
      sql"""
        INSERT INTO observation (observation_id,
                                program_id,
                                observation_index,
                                title,
                                instrument)
              VALUES (${oid},
                      ${oid.pid},
                      ${oid.index},
                      ${o.title},
                      ${o.staticConfig.instrument: Instrument})
      """.update

    def selectIds(pid: Program.Id): Query0[Observation.Id] =
      sql"""
        SELECT observation_id
          FROM observation
         WHERE program_id = $pid
      """.query[Observation.Id]

    def selectFlat(id: Observation.Id): Query0[Observation[Instrument, Nothing]] =
      sql"""
        SELECT title, instrument
          FROM observation
         WHERE observation_id = ${id}
      """.query[(String, Instrument)]
        .map { case (t, i) => Observation(t, TargetEnvironment.empty, i, Nil) }

    def selectAllFlat(pid: Program.Id): Query0[(Observation.Index, Observation[Instrument, Nothing])] =
      sql"""
        SELECT observation_index, title, instrument
          FROM observation
         WHERE program_id = ${pid}
      ORDER BY observation_index
      """.query[(Short, String, Instrument)]
        .map { case (n, t, i) =>
          (Observation.Index.unsafeFromShort(n), Observation(t, TargetEnvironment.empty, i, Nil))
        }

  }
}
