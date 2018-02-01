// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import cats.implicits._
import doobie._, doobie.implicits._
import gem.config.StaticConfig
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
  def insert(oid: Observation.Id, o: Observation.Full): ConnectionIO[Unit] =
    for {
      _ <- Statements.insert(oid, o).run
      _ <- StaticConfigDao.insert(oid, o.staticConfig)
      _ <- TargetEnvironmentDao.insert(oid, o.targets)
      _ <- o.steps.zipWithIndex.traverse { case (s, i) =>
             StepDao.insert(oid, Location.unsafeMiddle((i + 1) * 100), s)
           }.void
    } yield ()

  /** Construct a program to select the specified observation, with the
    * instrument but not targets nor steps.
    */
  def selectFlat(id: Observation.Id): ConnectionIO[Observation[Unit, Instrument, Nothing]] =
    Statements.selectFlat(id).unique

  /** Construct a program to select the specified observation, with the
    * targets and instrument type but not steps.
    */
  def selectTargets(id: Observation.Id): ConnectionIO[Observation[TargetEnvironment, Instrument, Nothing]] =
    for {
      o <- selectFlat(id)
      t <- TargetEnvironmentDao.selectObs(id)
    } yield Observation.targetsFunctor.as(o, t)

  /** Construct a program to select the specified observation, with static
    * config but not targets nor steps.
    */
  def selectStatic(id: Observation.Id): ConnectionIO[Observation[Unit, StaticConfig, Nothing]] =
    for {
      obs <- selectFlat(id)
      sc  <- StaticConfigDao.select(id, obs.staticConfig)
    } yield Observation.staticConfigFunctor.as(obs, sc)

  /** Construct a program to select a fully specified observation, with targets,
    * static config and steps.
    */
  def select(id: Observation.Id): ConnectionIO[Observation.Full] =
    for {
      on <- selectStatic(id)
      ss <- StepDao.selectAll(id)
      t  <- TargetEnvironmentDao.selectObs(id)
    } yield Observation.targetsFunctor.as(on.copy(steps = ss.values.toList), t)

  /** Construct a program to select the all obseravation ids for the specified
    * science program.
    */
  def selectIds(pid: Program.Id): ConnectionIO[List[Observation.Id]] =
    Statements.selectIds(pid).list

  // Merges a map of Index -> TargetEnvironment into a matching map of
  // Index -> Observation.
  private def merge[I: Ordering, T, S, D](
    os: TreeMap[I, Observation[T, S, D]],
    ts: Map[I, TargetEnvironment]
  ): TreeMap[I, Observation[TargetEnvironment, S, D]] =

    os.foldLeft(TreeMap.empty[I, Observation[TargetEnvironment, S, D]]) {
      case (m, (i, o)) =>
        m.updated(i, Observation.targetsFunctor.as(
          o, ts.getOrElse(i, TargetEnvironment.empty)
        ))
    }

  /** Construct a program to select all observations for the specified science
    * program, with the instrument but no targets nor steps.
    */
  def selectAllFlat(pid: Program.Id): ConnectionIO[TreeMap[Observation.Index, Observation[Unit, Instrument, Nothing]]] =
    Statements.selectAllFlat(pid).list.map(lst => TreeMap.fromList(lst))

  /** Construct a program to select all observations for the specified science
    * program, with the targets and the instrument type, but no steps.
    */
  def selectAllTarget(pid: Program.Id): ConnectionIO[TreeMap[Observation.Index, Observation[TargetEnvironment, Instrument, Nothing]]] =
    for {
      m  <- selectAllFlat(pid)
      ts <- TargetEnvironmentDao.selectProg(pid)
    } yield merge(m, ts)

  /** Construct a program to select all observations for the specified science
    * program, with the static component but no targets nor steps.
    */
  def selectAllStatic(pid: Program.Id): ConnectionIO[TreeMap[Observation.Index, Observation[Unit, StaticConfig, Nothing]]] =
    for {
      ids <- selectIds(pid)
      oss <- ids.traverse(selectStatic)
    } yield TreeMap.fromList(ids.map(_.index).zip(oss))

  /** Construct a program to select all observations for the specified science
    * program, with its targets, static component and steps.
    */
  def selectAll(pid: Program.Id): ConnectionIO[TreeMap[Observation.Index, Observation.Full]] =
    for {
      ids <- selectIds(pid)
      oss <- ids.traverse(select)
      ts  <- TargetEnvironmentDao.selectProg(pid)
    } yield merge(TreeMap.fromList(ids.map(_.index).zip(oss)), ts)

  object Statements {

    def insert(oid: Observation.Id, o: Observation[_, StaticConfig, _]): Update0 =
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

    def selectFlat(id: Observation.Id): Query0[Observation[Unit, Instrument, Nothing]] =
      sql"""
        SELECT title, instrument
          FROM observation
         WHERE observation_id = ${id}
      """.query[(String, Instrument)]
        .map { case (t, i) => Observation(t, (), i, Nil) }

    def selectAllFlat(pid: Program.Id): Query0[(Observation.Index, Observation[Unit, Instrument, Nothing])] =
      sql"""
        SELECT observation_index, title, instrument
          FROM observation
         WHERE program_id = ${pid}
      ORDER BY observation_index
      """.query[(Short, String, Instrument)]
        .map { case (n, t, i) =>
          (Observation.Index.unsafeFromShort(n), Observation(t, (), i, Nil))
        }

  }
}
