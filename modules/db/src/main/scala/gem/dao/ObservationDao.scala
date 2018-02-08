// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import cats.implicits._
import doobie._, doobie.implicits._
import gem.config.{ DynamicConfig, StaticConfig }
import gem.dao.meta._
import gem.enum._
import gem.syntax.treemap._
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
  def selectFlat(id: Observation.Id): ConnectionIO[Observation[Option[AsterismType], Instrument, Nothing]] =
    Statements.selectFlat(id).unique

  /** Construct a program to select the specified observation, with the
    * targets and instrument type but not steps.
    */
  def selectTargets(id: Observation.Id): ConnectionIO[Observation[TargetEnvironment, Instrument, Nothing]] =
    for {
      o <- selectFlat(id)
      t <- TargetEnvironmentDao.selectObs(id, o.targets)
    } yield Observation.targetsFunctor.as(o, t)

  /** Construct a program to select the specified observation, with static
    * config but not targets nor steps.
    */
  def selectStatic(id: Observation.Id): ConnectionIO[Observation[Option[AsterismType], StaticConfig, Nothing]] =
    for {
      obs <- selectFlat(id)
      sc  <- StaticConfigDao.select(id, obs.staticConfig)
    } yield Observation.staticConfigFunctor.as(obs, sc)

  /** Construct a program to select the specified observation, with static
    * config and steps but not targets.
    */
  def selectConfig(id: Observation.Id): ConnectionIO[Observation[Option[AsterismType], StaticConfig, Step[DynamicConfig]]] =
    for {
      on <- selectStatic(id)
      ss <- StepDao.selectAll(id)
    } yield on.copy(steps = ss.values.toList)

  /** Construct a program to select a fully specified observation, with targets,
    * static config and steps.
    */
  def select(id: Observation.Id): ConnectionIO[Observation.Full] =
    for {
      o <- selectConfig(id)
      t <- TargetEnvironmentDao.selectObs(id, o.targets)
    } yield Observation.targetsFunctor.as(o, t)

  /** Construct a program to select the all obseravation ids for the specified
    * science program.
    */
  def selectIds(pid: Program.Id): ConnectionIO[List[Observation.Id]] =
    Statements.selectIds(pid).to[List]

  // Merges a map of Index -> TargetEnvironment into a matching map of
  // Index -> Observation.
  private def merge[I: Ordering, T, S, D](
    os: TreeMap[I, Observation[T, S, D]],
    ts: Map[I, TargetEnvironment]
  ): TreeMap[I, Observation[TargetEnvironment, S, D]] =

    os.mergeMatchingKeys(ts)((o, t) =>
      Observation.targetsFunctor.as(o, t.getOrElse(TargetEnvironment.empty))
    )

  /** Construct a program to select all observations for the specified science
    * program, with the instrument but no targets nor steps.
    */
  def selectAllFlat(pid: Program.Id): ConnectionIO[TreeMap[Observation.Index, Observation[Option[AsterismType], Instrument, Nothing]]] =
    Statements.selectAllFlat(pid).to[List].map(lst => TreeMap.fromList(lst))

  /** Construct a program to select all observations for the specified science
    * program, with the targets and the instrument type, but no steps.
    */
  def selectAllTarget(pid: Program.Id): ConnectionIO[TreeMap[Observation.Index, Observation[TargetEnvironment, Instrument, Nothing]]] =
    for {
      os  <- selectAllFlat(pid)
      as  = os.values.toList.flatMap(_.targets.toList).toSet // All asterism types in the program
      ts <- TargetEnvironmentDao.selectProg(pid, as)
    } yield merge(os, ts)

  /** Construct a program to select all observations for the specified science
    * program, with the static component but no targets nor steps.
    */
  def selectAllStatic(pid: Program.Id): ConnectionIO[TreeMap[Observation.Index, Observation[Option[AsterismType], StaticConfig, Nothing]]] =
    for {
      ids <- selectIds(pid)
      oss <- ids.traverse(selectStatic)
    } yield TreeMap.fromList(ids.map(_.index).zip(oss))

  /** Construct a program to select all observations for the specified science
    * program, with static component and steps but not targets.
    */
  def selectAllConfig(pid: Program.Id): ConnectionIO[TreeMap[Observation.Index, Observation[Option[AsterismType], StaticConfig, Step[DynamicConfig]]]] =
    for {
      ids <- selectIds(pid)
      oss <- ids.traverse(selectConfig)
    } yield TreeMap.fromList(ids.map(_.index).zip(oss))

  /** Construct a program to select all observations for the specified science
    * program, with its targets, static component and steps.
    */
  def selectAll(pid: Program.Id): ConnectionIO[TreeMap[Observation.Index, Observation.Full]] =
    for {
      os <- selectAllConfig(pid)
      as  = os.values.toList.flatMap(_.targets.toList).toSet // All asterism types in the program
      ts <- TargetEnvironmentDao.selectProg(pid, as)
    } yield merge(os, ts)

  object Statements {

    import AsterismTypeMeta._

    def insert(oid: Observation.Id, o: Observation[TargetEnvironment, StaticConfig, _]): Update0 =
      sql"""
        INSERT INTO observation (observation_id,
                                program_id,
                                observation_index,
                                asterism_type,
                                title,
                                instrument)
              VALUES (${oid},
                      ${oid.pid},
                      ${oid.index},
                      ${o.targets.asterism.map(AsterismType.of)},
                      ${o.title},
                      ${o.staticConfig.instrument: Instrument})
      """.update

    def selectIds(pid: Program.Id): Query0[Observation.Id] =
      sql"""
        SELECT observation_id
          FROM observation
         WHERE program_id = $pid
      """.query[Observation.Id]

    def selectFlat(id: Observation.Id): Query0[Observation[Option[AsterismType], Instrument, Nothing]] =
      sql"""
        SELECT title, asterism_type, instrument
          FROM observation
         WHERE observation_id = ${id}
      """.query[(String, Option[AsterismType], Instrument)]
        .map { case (t, a, i) => Observation(t, a, i, Nil) }

    def selectAllFlat(pid: Program.Id): Query0[(Observation.Index, Observation[Option[AsterismType], Instrument, Nothing])] =
      sql"""
        SELECT observation_index, title, asterism_type, instrument
          FROM observation
         WHERE program_id = ${pid}
      ORDER BY observation_index
      """.query[(Short, String, Option[AsterismType], Instrument)]
        .map { case (n, t, a, i) =>
          (Observation.Index.unsafeFromShort(n), Observation(t, a, i, Nil))
        }

  }
}
