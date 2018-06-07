// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import cats.implicits._
import doobie._, doobie.implicits._
import gem.config.{ StaticConfig }
import gem.dao.meta._
import gem.enum._
import gem.math.Index
import gem.syntax.treemap._
import gem.util.Location

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

  /** Construct a program to select the specified observation, with the
    * instrument but not targets nor steps.
    */
  def selectFlat(id: Observation.Id): ConnectionIO[(String, Option[AsterismType], Instrument)] =
    Statements.selectFlat(id).unique

  /** Construct a program to select the specified observation, with the
    * targets and instrument type but not steps.
    */
  def selectTargets(id: Observation.Id): ConnectionIO[(String, TargetEnvironment)] =
    for {
      o <- selectFlat(id)
      t <- TargetEnvironmentDao.selectObs(id)
    } yield (o._1, t)

  /** Construct a program to select the specified observation, with static
    * config but not targets nor steps.
    */
  def selectStatic(id: Observation.Id): ConnectionIO[(String, Option[AsterismType], StaticConfig)] =
    for {
      o <- selectFlat(id)
      c <- StaticConfigDao.select(id, o._3)
    } yield (o._1, o._2, c)

  /** Construct a program to select the specified observation, with static
    * config and steps but not targets.
    */
  def selectConfig(id: Observation.Id): ConnectionIO[(String, Option[AsterismType], StaticConfig, TreeMap[Location.Middle, Step])] =
    for {
      o  <- selectStatic(id)
      ss <- StepDao.selectAll(id)
    } yield (o._1, o._2, o._3, ss)

  /** Construct a program to select a fully specified observation, with targets,
    * static config and steps.
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def select(id: Observation.Id): ConnectionIO[Observation] =
    for {
      o <- selectConfig(id)
      t <- TargetEnvironmentDao.selectObs(id)
    } yield assemble(o._1, t, o._3, o._4.values.toList)

  private implicit class SequenceOps(ss: List[Step]) {
    def narrow[A](pf: PartialFunction[Step, A]): List[A] =
      ss.collect(pf orElse {
        case _ => sys.error("inconsistent sequence contains multiple instruments")
      })
  }

  /** Assemble an Observation from its parts, assuming they're consistent. */
  private def assemble(
    title: String,
    targetEnvironment: TargetEnvironment,
    staticConfig: StaticConfig,
    sequence: List[Step]
  ): Observation =
    (targetEnvironment, staticConfig) match {
      case (te: TargetEnvironment.Phoenix,    sc: StaticConfig.Phoenix)    => Observation.Phoenix(   title, te, sc, sequence.narrow { case s: Step.Phoenix    => s })
      case (te: TargetEnvironment.Michelle,   sc: StaticConfig.Michelle)   => Observation.Michelle(  title, te, sc, sequence.narrow { case s: Step.Michelle   => s })
      case (te: TargetEnvironment.Gnirs,      sc: StaticConfig.Gnirs)      => Observation.Gnirs(     title, te, sc, sequence.narrow { case s: Step.Gnirs      => s })
      case (te: TargetEnvironment.Niri,       sc: StaticConfig.Niri)       => Observation.Niri(      title, te, sc, sequence.narrow { case s: Step.Niri       => s })
      case (te: TargetEnvironment.Trecs,      sc: StaticConfig.Trecs)      => Observation.Trecs(     title, te, sc, sequence.narrow { case s: Step.Trecs      => s })
      case (te: TargetEnvironment.Nici,       sc: StaticConfig.Nici)       => Observation.Nici(      title, te, sc, sequence.narrow { case s: Step.Nici       => s })
      case (te: TargetEnvironment.Nifs,       sc: StaticConfig.Nifs)       => Observation.Nifs(      title, te, sc, sequence.narrow { case s: Step.Nifs       => s })
      case (te: TargetEnvironment.Gpi,        sc: StaticConfig.Gpi)        => Observation.Gpi(       title, te, sc, sequence.narrow { case s: Step.Gpi        => s })
      case (te: TargetEnvironment.Gsaoi,      sc: StaticConfig.Gsaoi)      => Observation.Gsaoi(     title, te, sc, sequence.narrow { case s: Step.Gsaoi      => s })
      case (te: TargetEnvironment.GmosS,      sc: StaticConfig.GmosS)      => Observation.GmosS(     title, te, sc, sequence.narrow { case s: Step.GmosS      => s })
      case (te: TargetEnvironment.AcqCam,     sc: StaticConfig.AcqCam)     => Observation.AcqCam(    title, te, sc, sequence.narrow { case s: Step.AcqCam     => s })
      case (te: TargetEnvironment.GmosN,      sc: StaticConfig.GmosN)      => Observation.GmosN(     title, te, sc, sequence.narrow { case s: Step.GmosN      => s })
      case (te: TargetEnvironment.Bhros,      sc: StaticConfig.Bhros)      => Observation.Bhros(     title, te, sc, sequence.narrow { case s: Step.Bhros      => s })
      case (te: TargetEnvironment.Visitor,    sc: StaticConfig.Visitor)    => Observation.Visitor(   title, te, sc, sequence.narrow { case s: Step.Visitor    => s })
      case (te: TargetEnvironment.Flamingos2, sc: StaticConfig.Flamingos2) => Observation.Flamingos2(title, te, sc, sequence.narrow { case s: Step.Flamingos2 => s })
      case (te: TargetEnvironment.Ghost,      sc: StaticConfig.Ghost)      => Observation.Ghost(     title, te, sc, sequence.narrow { case s: Step.Ghost      => s })
      case _ => sys.error("inconsistent observation")
    }

  /** Construct a program to select the all obseravation ids for the specified
    * science program.
    */
  def selectIds(pid: Program.Id): ConnectionIO[List[Observation.Id]] =
    Statements.selectIds(pid).to[List]

  /** Construct a program to select all observations for the specified science
    * program, with the instrument but no targets nor steps.
    */
  def selectAllFlat(pid: Program.Id): ConnectionIO[TreeMap[Index, (String, Option[AsterismType], Instrument)]] =
    Statements.selectAllFlat(pid)
      .map { case (a, b, c, d) => (a, (b, c, d)) } // :-\
      .to[List]
      .map(TreeMap.fromList(_))

  /** Construct a program to select all observations for the specified science
    * program, with the targets and the instrument type, but no steps.
    */
  def selectAllTarget(pid: Program.Id): ConnectionIO[TreeMap[Index, (String, TargetEnvironment, Instrument)]] =
    (selectAllFlat(pid), TargetEnvironmentDao.selectProg(pid)).mapN { (rm, tm) =>
      rm.map { case (idx, (s, _, i)) =>
        idx -> ((s, tm(idx), i))
      }
    }

  /** Construct a program to select all observations for the specified science
    * program, with the static component but no targets nor steps.
    */
  def selectAllStatic(pid: Program.Id): ConnectionIO[TreeMap[Index, (String, Option[AsterismType], StaticConfig)]] =
    for {
      ids <- selectIds(pid)
      oss <- ids.traverse(selectStatic)
    } yield TreeMap.fromList(ids.map(_.index).zip(oss))

  /** Construct a program to select all observations for the specified science
    * program, with static component and steps but not targets.
    */
  def selectAllConfig(pid: Program.Id): ConnectionIO[TreeMap[Index, (String, Option[AsterismType], StaticConfig, TreeMap[Location.Middle, Step])]] =
    for {
      ids <- selectIds(pid)
      oss <- ids.traverse(selectConfig)
    } yield TreeMap.fromList(ids.map(_.index).zip(oss))

  /** Construct a program to select all observations for the specified science
    * program, with its targets, static component and steps.
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def selectAll(pid: Program.Id): ConnectionIO[TreeMap[Index, Observation]] =
    (selectAllConfig(pid), TargetEnvironmentDao.selectProg(pid)).mapN { (rm, tm) =>
      rm.map {  case (idx, (t, _, sc, seq)) =>
        idx -> assemble(t, tm(idx), sc, seq.values.toList)
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

    def selectFlat(id: Observation.Id): Query0[(String, Option[AsterismType], Instrument)] =
      sql"""
        SELECT title, asterism_type, instrument
          FROM observation
         WHERE observation_id = ${id}
      """.query[(String, Option[AsterismType], Instrument)]

    def selectAllFlat(pid: Program.Id): Query0[(Index, String, Option[AsterismType], Instrument)] =
      sql"""
        SELECT observation_index, title, asterism_type, instrument
          FROM observation
         WHERE program_id = ${pid}
      ORDER BY observation_index
      """.query[(Short, String, Option[AsterismType], Instrument)]
        .map { case (n, t, a, i) =>
          (Index.fromShort.unsafeGet(n), t, a, i)
        }

  }
}
