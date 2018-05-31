// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import cats.implicits._

import doobie._
import doobie.implicits._

import gem.dao.meta._
import gem.enum.{ AsterismType, Instrument }
import gem.math.Index
import scala.collection.immutable.TreeMap


object AsterismDao {

  private val NoTarget: ConnectionIO[Option[Target]] =
    Option.empty[Target].pure[ConnectionIO]

  def insert(oid: Observation.Id, a: Asterism): ConnectionIO[Unit] =
    a match {
      case a: Asterism.SingleTarget => insertSingleTarget(oid, a)
      case a: Asterism.GhostDualTarget => insertDualTarget(oid, a)
    }

  def select(oid: Observation.Id, t: AsterismType): ConnectionIO[Option[Asterism]] =
    t match {
      case AsterismType.SingleTarget    => selectSingleTarget(oid)
      case AsterismType.GhostDualTarget => selectDualTarget(oid)
    }

  def selectAll(pid: Program.Id, t: AsterismType): ConnectionIO[TreeMap[Index, Asterism]] =
    t match {
      case AsterismType.SingleTarget    => selectAllSingleTarget(pid)
      case AsterismType.GhostDualTarget => TreeMap.empty[Index, Asterism].pure[ConnectionIO]
    }

  def insertSingleTarget(oid: Observation.Id, a: Asterism.SingleTarget): ConnectionIO[Unit] =
    for {
      t <- TargetDao.insert(a.target)
      _ <- Statements.SingleTarget.insert(oid, t, Instrument.forAsterism(a)).run.void
    } yield ()

  def insertDualTarget(oid: Observation.Id, a: Asterism.GhostDualTarget): ConnectionIO[Unit] =
    for {
      t0 <- TargetDao.insert(a.ifu1)
      t1 <- TargetDao.insert(a.ifu2)
      _ <- Statements.GhostDualTarget.insert(oid, t0, t1).run.void
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

  def selectSingleTarget(oid: Observation.Id): ConnectionIO[Option[Asterism]] =
    for {
      s <- Statements.SingleTarget.select(oid).option
      t <- s.fold(NoTarget) { case (id, _) => TargetDao.select(id) }
    } yield t.product(s).map { case (target, (_, inst)) => unsafeToSingleTargetAsterism(target, inst) }

  def selectDualTarget(oid: Observation.Id): ConnectionIO[Option[Asterism]] =
    for {
      ids <- Statements.GhostDualTarget.select(oid).option
      t0  <- ids.fold(NoTarget) { case (id, _) => TargetDao.select(id) }
      t1  <- ids.fold(NoTarget) { case (_, id) => TargetDao.select(id) }
    } yield t0.product(t1).map { case (t0, t1) => Asterism.GhostDualTarget(t0, t1) }

  type AsterismMap = ConnectionIO[TreeMap[Index, Asterism]]
  type OptMapEntry = ConnectionIO[Option[(Index, Asterism)]]

  private def toAsterismMap[A](as: List[A])(f: A => OptMapEntry): AsterismMap =
    as.traverse(f).map { lst => TreeMap(lst.flatMap(_.toList): _*) }

  def selectAllSingleTarget(pid: Program.Id): AsterismMap = {
    def toEntry[I <: Instrument with Singleton](idx: Index, tid: Target.Id, inst: Instrument.Aux[I]): OptMapEntry =
      TargetDao.select(tid).map {
        _.map(idx -> unsafeToSingleTargetAsterism(_, inst))
      }

    for {
      l <- Statements.SingleTarget.selectAll(pid).to[List]
      m <- toAsterismMap(l) { case (idx, tid, inst) => toEntry(idx, tid, inst: Instrument.Aux[inst.type]) }
    } yield m
  }

  def selectAllDualTarget(pid: Program.Id): AsterismMap = {
    def toEntry(idx: Index, tid0: Target.Id, tid1: Target.Id): OptMapEntry =
      for {
        t0 <- TargetDao.select(tid0)
        t1 <- TargetDao.select(tid1)
      } yield t0.product(t1).map { case (t0, t1) => idx -> Asterism.GhostDualTarget(t0, t1) }

    for {
      l <- Statements.GhostDualTarget.selectAll(pid).to[List]
      m <- toAsterismMap(l) { case (idx, t0, t1) => toEntry(idx, t0, t1) }
    } yield m
  }

  object Statements {

    import AsterismTypeMeta._
    import EnumeratedMeta._
    import ProgramIdMeta._
    import IndexMeta._

    object SingleTarget {

      def insert(oid: Observation.Id, t: Target.Id, i: Instrument): Update0 =
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

      def select(oid: Observation.Id): Query0[(Target.Id, Instrument)] =
        sql"""
          SELECT target_id,
                 instrument
            FROM single_target_asterism
           WHERE program_id        = ${oid.pid}
             AND observation_index = ${oid.index}
        """.query[(Target.Id, Instrument)]

      def selectAll(pid: Program.Id): Query0[(Index, Target.Id, Instrument)] =
        sql"""
          SELECT observation_index,
                 target_id,
                 instrument
            FROM single_target_asterism
           WHERE program_id = $pid
        """.query[(Index, Target.Id, Instrument)]
    }

    object GhostDualTarget {

      def insert(oid: Observation.Id, t0: Target.Id, t1: Target.Id): Update0 =
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

      def select(oid: Observation.Id): Query0[(Target.Id, Target.Id)] =
        sql"""
          SELECT target1_id,
                 target2_id
            FROM ghost_dual_target_asterism
           WHERE program_id        = ${oid.pid}
             AND observation_index = ${oid.index}
        """.query[(Target.Id, Target.Id)]

      def selectAll(pid: Program.Id): Query0[(Index, Target.Id, Target.Id)] =
        sql"""
          SELECT observation_index,
                 target1_id,
                 target2_id
            FROM ghost_dual_target_asterism
           WHERE program_id = $pid
        """.query[(Index, Target.Id, Target.Id)]

    }

  }
}