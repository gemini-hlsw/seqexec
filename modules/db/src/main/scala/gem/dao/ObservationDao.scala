// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import cats.implicits._
import doobie._, doobie.implicits._
import gem.config.{ StaticConfig }
import gem.dao.meta._
import gem.enum._
import gem.util.Location
import gsp.math.Index
import gsp.math.syntax.treemap._

import scala.collection.immutable.TreeMap

object ObservationDao {
  import EnumeratedMeta._
  import ObservationIdMeta._
  import IndexMeta._
  import ProgramIdMeta._

  /**
   * Construct a program to insert a fully-populated Observation. This program will raise a
   * key violation if an observation with the same id already exists.
   */
  def insert(oid: Observation.Id, o: Observation): ConnectionIO[Unit] =
    for {
      _ <- Statements.insert(oid, o).run
      _ <- StaticConfigDao.insert(oid, o.staticConfig)
      _ <- TargetEnvironmentDao.insert(oid, o.targetEnvironment)
      _ <- o.sequence.zipWithIndex.traverse { case (s, i) =>
             StepDao.insert(oid, Location.unsafeMiddle((i + 1) * 100), s)
           }.void
    } yield ()

  /**
   * Construct a program to select the specified observation, with the
   * instrument but not targets nor steps.  Raises an exception if the indicated
   * observation does not exist.
   */
  def fetchFlat(id: Observation.Id): ConnectionIO[(String, Instrument)] =
    Statements.selectFlat(id).unique

  /**
   * Construct a program to query for the specified observation, with the
   * instrument but not targets nor steps.
   */
  def queryFlat(id: Observation.Id): ConnectionIO[Option[(String, Instrument)]] =
    Statements.selectFlat(id).option

  /**
   * Construct a program to select the specified observation, with the
   * targets and instrument type but not steps.  Raises an exception if the
   * indicated observation does not exist.
   */
  def fetchTargets(id: Observation.Id): ConnectionIO[(String, TargetEnvironment)] =
    for {
      o <- fetchFlat(id)
      t <- TargetEnvironmentDao.selectObs(id)
    } yield (o._1, t)

  /**
   * Construct a program to query for the specified observation, with the
   * targets and instrument type but not steps.
   */
  def queryTargets(id: Observation.Id): ConnectionIO[Option[(String, TargetEnvironment)]] =
    for {
      o <- queryFlat(id)
      t <- o.as(TargetEnvironmentDao.selectObs(id)).sequence
    } yield o.map(_._1).product(t)

  /**
   * Construct a program to select the specified observation, with static
   * config but not targets nor steps.  Raises an exception if the indicated
   * observation does not exist.
   */
  def fetchStatic(id: Observation.Id): ConnectionIO[(String, StaticConfig)] =
    for {
      o <- fetchFlat(id)
      c <- StaticConfigDao.select(id, o._2)
    } yield (o._1, c)

  /**
   * Construct a program to query for the specified observation, with static
   * config but not targets nor steps.
   */
  def queryStatic(id: Observation.Id): ConnectionIO[Option[(String, StaticConfig)]] =
    for {
      o <- queryFlat(id)
      c <- o.traverse(f => StaticConfigDao.select(id, f._2))
    } yield o.map(_._1).product(c)

  /**
   * Construct a program to select the specified observation, with static
   * config and steps but not targets.  Raises an exception if the indicated
   * observation does not exist.
   */
  def fetchConfig(id: Observation.Id): ConnectionIO[(String, StaticConfig, TreeMap[Location.Middle, Step])] =
    for {
      o  <- fetchStatic(id)
      ss <- StepDao.selectAll(id)
    } yield (o._1, o._2, ss)

  /**
   * Construct a program to query for the specified observation, with static
   * config and steps but not targets.
   */
  def queryConfig(id: Observation.Id): ConnectionIO[Option[(String, StaticConfig, TreeMap[Location.Middle, Step])]] =
    for {
      o  <- queryStatic(id)
      ss <- StepDao.selectAll(id)
    } yield o.map(c => (c._1, c._2, ss))

  /**
   * Construct a program to select a fully specified observation, with targets,
   * static config and steps.  Raises an exception if the indicated observation
   * does not exist.
   */
  def fetch(id: Observation.Id): ConnectionIO[Observation] =
    for {
      o <- fetchConfig(id)
      t <- TargetEnvironmentDao.selectObs(id)
    } yield Observation.unsafeAssemble(o._1, t, o._2, o._3.values.toList)

  /**
   * Construct a program to query for a fully specified observation, with
   * targets, static config and steps.
   */
  def query(id: Observation.Id): ConnectionIO[Option[Observation]] =
    for {
      o <- queryConfig(id)
      t <- o.as(TargetEnvironmentDao.selectObs(id)).sequence
    } yield o.map2(t) { (oʹ, tʹ) =>
      Observation.unsafeAssemble(oʹ._1, tʹ, oʹ._2, oʹ._3.values.toList)
    }

  /**
   * Construct a program to query for all obseravation ids for the specified
   * science program.
   */
  def queryIds(pid: Program.Id): ConnectionIO[List[Observation.Id]] =
    Statements.selectIds(pid).to[List]

  /**
   * Construct a program to query for all observations for the specified science
   * program, with the instrument but no targets nor steps.
   */
  def queryAllFlat(pid: Program.Id): ConnectionIO[TreeMap[Index, (String, Instrument)]] =
    Statements.selectAllFlat(pid)
      .map { case (a, b, c) => (a, (b, c)) } // :-\
      .to[List]
      .map(TreeMap.fromList(_))

  /**
   * Construct a program to query for all observations for the specified science
   * program, with the targets and the instrument type, but no steps.
   */
  def queryAllTarget(pid: Program.Id): ConnectionIO[TreeMap[Index, (String, TargetEnvironment, Instrument)]] =
    (queryAllFlat(pid), TargetEnvironmentDao.selectProg(pid)).mapN { (rm, tm) =>
      rm.map { case (idx, (s, i)) =>
        idx -> ((s, tm(idx), i))
      }
    }

  /**
   * Construct a program to query for all observations for the specified science
   * program, with the static component but no targets nor steps.
   */
  def queryAllStatic(pid: Program.Id): ConnectionIO[TreeMap[Index, (String, StaticConfig)]] =
    for {
      ids <- queryIds(pid)
      oss <- ids.traverse(fetchStatic)
    } yield TreeMap.fromList(ids.map(_.index).zip(oss))

  /**
   * Construct a program to query for all observations for the specified science
   * program, with static component and steps but not targets.
   */
  def queryAllConfig(pid: Program.Id): ConnectionIO[TreeMap[Index, (String, StaticConfig, TreeMap[Location.Middle, Step])]] =
    for {
      ids <- queryIds(pid)
      oss <- ids.traverse(fetchConfig)
    } yield TreeMap.fromList(ids.map(_.index).zip(oss))

  /**
   * Construct a program to query for all observations for the specified science
   * program, with its targets, static component and steps.
   */
  def queryAll(pid: Program.Id): ConnectionIO[TreeMap[Index, Observation]] =
    (queryAllConfig(pid), TargetEnvironmentDao.selectProg(pid)).mapN { (rm, tm) =>
      rm.map {  case (idx, (t, sc, seq)) =>
        idx -> Observation.unsafeAssemble(t, tm(idx), sc, seq.values.toList)
      }
    }

  object Statements {

    import AsterismTypeMeta._

    def insert(oid: Observation.Id, o: Observation): Update0 =
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
                      ${o.targetEnvironment.asterism.map(AsterismType.of)},
                      ${o.title},
                      ${Instrument.forObservation(o)})
      """.update

    def selectIds(pid: Program.Id): Query0[Observation.Id] =
      sql"""
        SELECT observation_id
          FROM observation
         WHERE program_id = $pid
      """.query[Observation.Id]

    def selectFlat(id: Observation.Id): Query0[(String, Instrument)] =
      sql"""
        SELECT title, instrument
          FROM observation
         WHERE observation_id = ${id}
      """.query[(String, Instrument)]

    def selectAllFlat(pid: Program.Id): Query0[(Index, String, Instrument)] =
      sql"""
        SELECT observation_index, title, instrument
          FROM observation
         WHERE program_id = ${pid}
      ORDER BY observation_index
      """.query[(Short, String, Instrument)]
        .map { case (n, t, i) =>
          (Index.fromShort.unsafeGet(n), t, i)
        }

  }
}
