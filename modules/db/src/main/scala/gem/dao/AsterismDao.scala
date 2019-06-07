// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import cats.implicits._

import doobie._
import doobie.implicits._

import gem.dao.meta._
import gem.enum.{ AsterismType, Instrument }
import gem.instances.treemap._
import gsp.math.Index
import gsp.math.syntax.treemap._
import scala.collection.immutable.{ TreeMap }

object AsterismDao {

  def insert(oid: Observation.Id, a: Asterism): ConnectionIO[Unit] =
    a match {
      case a: Asterism.SingleTarget    => insertSingleTarget(oid, a)
      case a: Asterism.GhostDualTarget => insertDualTarget(oid, a)
    }

  /** Select the asterism for the given observation, if any, otherwise return the Instrument, which
    * is necessary knowledge when constructing a TargetEnvironment with an empty asterism. An
    * exception is raised if the specified observation does not exist.
    */
  def select(oid: Observation.Id): ConnectionIO[Either[Asterism, Instrument]] =
    Statements.select(oid).unique.flatMap(promote)

  /** Analogous to `select` but selects all observations in the specified program, if any, yielding
    * a map from `Index` to `Asterism`, or `Instrument` when there is no asterism.
    */
  def selectAll(pid: Program.Id): ConnectionIO[TreeMap[Index, Either[Asterism, Instrument]]] =
    Statements.selectAll(pid).to[List].flatMap { pairs =>
      TreeMap.fromList(pairs).traverse(promote)
    }

  private def insertSingleTarget(oid: Observation.Id, a: Asterism.SingleTarget): ConnectionIO[Unit] =
    for {
      t <- TargetDao.insert(a.target)
      _ <- Statements.insertSingleTarget(oid, t, Instrument.forAsterism(a)).run.void
    } yield ()

  private def insertDualTarget(oid: Observation.Id, a: Asterism.GhostDualTarget): ConnectionIO[Unit] =
    for {
      t0 <- TargetDao.insert(a.ifu1)
      t1 <- TargetDao.insert(a.ifu2)
      _  <- Statements.insertGhostDualTarget(oid, t0, t1).run.void
    } yield ()

  private def toSingleTargetAsterism(t: Target, i: Instrument): Option[Asterism] =
    i match {
      case Instrument.Phoenix    => Some(Asterism.Phoenix(t))
      case Instrument.Michelle   => Some(Asterism.Michelle(t))
      case Instrument.Gnirs      => Some(Asterism.Gnirs(t))
      case Instrument.Niri       => Some(Asterism.Niri(t))
      case Instrument.Trecs      => Some(Asterism.Trecs(t))
      case Instrument.Nici       => Some(Asterism.Nici(t))
      case Instrument.Nifs       => Some(Asterism.Nifs(t))
      case Instrument.Gpi        => Some(Asterism.Gpi(t))
      case Instrument.Gsaoi      => Some(Asterism.Gsaoi(t))
      case Instrument.GmosS      => Some(Asterism.GmosS(t))
      case Instrument.AcqCam     => Some(Asterism.AcqCam(t))
      case Instrument.GmosN      => Some(Asterism.GmosN(t))
      case Instrument.Bhros      => Some(Asterism.Bhros(t))
      case Instrument.Visitor    => Some(Asterism.Visitor(t))
      case Instrument.Flamingos2 => Some(Asterism.Flamingos2(t))
      case Instrument.Ghost      => None
    }

  private def unsafeToSingleTargetAsterism(t: Target, i: Instrument): Asterism =
    toSingleTargetAsterism(t, i).getOrElse(sys.error(s"No single-target asterism available for $i"))

  // Promote a selected row in terms of ids into one in terms of asterisms
  private def promote(
    row: (Instrument, Option[Either[Target.Id, (Target.Id, Target.Id)]])
  ): ConnectionIO[Either[Asterism, Instrument]] =
    row match {

      // No asterism, return the instrument instead
      case (i, None) =>
        i.asRight[Asterism].pure[ConnectionIO]

      // Single target, ok for any instrument other than GHOST for now (checked in unsafeToSingleTargetAsterism)
      case (i, Some(Left(tid))) =>
        TargetDao.selectUnique(tid).map(unsafeToSingleTargetAsterism(_, i).asLeft[Instrument])

      // Dual target, ok only for GHOST
      case (Instrument.Ghost, Some(Right((tid1, tid2)))) =>
        (TargetDao.selectUnique(tid1), TargetDao.selectUnique(tid2)).mapN { (t1, t2) =>
          Asterism.GhostDualTarget(t1, t2).asLeft[Instrument]
        }

      // Inconsistent
      case x => sys.error(s"Inconsistent asterism result: $x")

    }

  object Statements {

    import AsterismTypeMeta._
    import EnumeratedMeta._
    import ProgramIdMeta._
    import IndexMeta._

    def insertSingleTarget(oid: Observation.Id, t: Target.Id, i: Instrument): Update0 =
      sql"""
        INSERT INTO single_target_asterism (
                      program_id,
                      observation_index,
                      instrument,
                      asterism_type,
                      target_id)
              VALUES (
                      ${oid.pid},
                      ${oid.index},
                      $i,
                      ${AsterismType.SingleTarget: AsterismType},
                      $t
                    )
      """.update

    def insertGhostDualTarget(oid: Observation.Id, t0: Target.Id, t1: Target.Id): Update0 =
      sql"""
        INSERT INTO ghost_dual_target_asterism (
                      program_id,
                      observation_index,
                      instrument,
                      asterism_type,
                      target1_id,
                      target2_id)
              VALUES (
                      ${oid.pid},
                      ${oid.index},
                      ${Instrument.Ghost: Instrument},
                      ${AsterismType.GhostDualTarget: AsterismType},
                      $t0,
                      $t1
                    )
      """.update

    def select(oid: Observation.Id): Query0[(Instrument, Option[Either[Target.Id, (Target.Id, Target.Id)]])] =
      sql"""
         SELECT o.instrument,
                o.asterism_type,
                sta.target_id,
                gdta.target1_id,
                gdta.target2_id
           FROM observation o
      LEFT JOIN single_target_asterism sta
             ON sta.program_id = o.program_id AND sta.observation_index = o.observation_index
      LEFT JOIN ghost_dual_target_asterism gdta
             ON gdta.program_id = o.program_id AND gdta.observation_index = o.observation_index
          WHERE o.program_id = ${oid.pid}
            AND o.observation_index = ${oid.index}
      """.query[(Instrument, Option[AsterismType], Option[Target.Id], Option[Target.Id], Option[Target.Id])].map {
        case (i, None, None, None, None)                                       => (i, None)
        case (i, Some(AsterismType.SingleTarget), Some(t), None, None)         => (i, Some(t.asLeft))
        case (i, Some(AsterismType.GhostDualTarget), None, Some(t1), Some(t2)) => (i, Some((t1, t2).asRight))
        case x => sys.error(s"Inconsistent asterism result: $x")
      }


    def selectAll(pid: Program.Id): Query0[(Index, (Instrument, Option[Either[Target.Id, (Target.Id, Target.Id)]]))] =
      sql"""
         SELECT o.observation_index,
                o.instrument,
                o.asterism_type,
                sta.target_id,
                gdta.target1_id,
                gdta.target2_id
           FROM observation o
      LEFT JOIN single_target_asterism sta
             ON sta.program_id = o.program_id AND sta.observation_index = o.observation_index
      LEFT JOIN ghost_dual_target_asterism gdta
             ON gdta.program_id = o.program_id AND gdta.observation_index = o.observation_index
          WHERE o.program_id = ${pid}
      """.query[(Index, (Instrument, Option[AsterismType], Option[Target.Id], Option[Target.Id], Option[Target.Id]))].map {
        case (n, (i, None, None, None, None) )                                      => (n, (i, None))
        case (n, (i, Some(AsterismType.SingleTarget), Some(t), None, None))         => (n, (i, Some(t.asLeft)))
        case (n, (i, Some(AsterismType.GhostDualTarget), None, Some(t1), Some(t2))) => (n, (i, Some((t1, t2).asRight)))
        case x => sys.error(s"Inconsistent asterism result: $x")
      }

  }
}