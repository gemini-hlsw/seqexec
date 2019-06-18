// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.dao.meta._
import gem.enum.UserTargetType
import gsp.math.Index
import gsp.math.syntax.treesetcompanion._

import cats.implicits._
import doobie._, doobie.implicits._

import scala.collection.immutable.TreeSet

object UserTargetDao {

  // A target ID and the corresponding user target type.  We use the id to
  // get the actual target.
  final case class ProtoUserTarget(targetId: Target.Id, targetType: UserTargetType, oi: Index) {

    val toUserTarget: ConnectionIO[Option[UserTarget]] =
      TargetDao.select(targetId).map { _.map(UserTarget(_, targetType)) }
  }

  import EnumeratedMeta._
  import ObservationIdMeta._

  def insert(oid: Observation.Id, userTarget: UserTarget): ConnectionIO[UserTarget.Id] =
    for {
      tid <- TargetDao.insert(userTarget.target)
      uid <- Statements.insert(tid, userTarget.targetType, oid)
                       .withUniqueGeneratedKeys[Int]("id")
                       .map(UserTarget.Id(_))
    } yield uid

  /** Selects the single `UserTarget` associated with the given id, if any. */
  def select(id: UserTarget.Id): ConnectionIO[Option[UserTarget]] =
    for {
      oput <- Statements.select(id).option
      out  <- oput.fold(Option.empty[UserTarget].pure[ConnectionIO]) { _.toUserTarget }
    } yield out

  private def selectAll(
    targetsQuery: Query0[(UserTarget.Id, ProtoUserTarget)]
  ): ConnectionIO[List[(Index, (UserTarget.Id, UserTarget))]] =
    for {
      puts <- targetsQuery.to[List]                              // List[(UserTarget.Id, ProtoUserTarget)]
      ots  <- puts.map(_._2.targetId).traverse(TargetDao.select) // List[Option[Target]]
    } yield puts.zip(ots).flatMap { case ((id, put), ot) =>
      ot.map(t => (put.oi, (id, UserTarget(t, put.targetType)))).toList
    }

  private def toUserTargetSet(lst: List[(UserTarget.Id, UserTarget)]): TreeSet[UserTarget] =
    TreeSet.fromList(lst.unzip._2)

  /** Selects all `UserTarget`s for an observation.
    */
  def selectObs(oid: Observation.Id): ConnectionIO[TreeSet[UserTarget]] =
    selectObsWithId(oid).map(toUserTargetSet)

  /** Selects all `UserTarget`s for an observation paired with the `UserTarget`
    * id itself.
    */
  def selectObsWithId(oid: Observation.Id): ConnectionIO[List[(UserTarget.Id, UserTarget)]] =
    selectAll(Statements.selectObs(oid)).map(_.unzip._2)

  /** Selects all `UserTarget`s for a program.
    */
  def selectProg(pid: Program.Id): ConnectionIO[Map[Index, TreeSet[UserTarget]]] =
    selectProgWithId(pid).map(_.mapValues(toUserTargetSet))

  /** Selects all `UserTarget`s for a program paired with the `UserTarget` id
    * itself.
    */
  def selectProgWithId(pid: Program.Id): ConnectionIO[Map[Index, List[(UserTarget.Id, UserTarget)]]] =
    selectAll(Statements.selectProg(pid)).map {
      _.groupBy(_._1).mapValues(_.unzip._2)
    }


  object Statements {

    import gem.dao.meta.ProgramIdMeta._
    import gem.dao.meta.IndexMeta._

    def insert(targetId: Target.Id, targetType: UserTargetType, oid: Observation.Id): Update0 =
      sql"""
        INSERT INTO user_target (
          target_id,
          user_target_type,
          program_id,
          observation_index,
          observation_id
        ) VALUES (
          $targetId,
          $targetType,
          ${oid.pid},
          ${oid.index},
          $oid
        )
      """.update

    def select(id: UserTarget.Id): Query0[ProtoUserTarget] =
      sql"""
        SELECT target_id,
               user_target_type,
               observation_index
          FROM user_target
         WHERE id = $id
      """.query[ProtoUserTarget]

    def selectObs(oid: Observation.Id): Query0[(UserTarget.Id, ProtoUserTarget)] =
      sql"""
        SELECT id,
               target_id,
               user_target_type,
               observation_index
          FROM user_target
         WHERE program_id = ${oid.pid} AND observation_index = ${oid.index}
      """.query[(UserTarget.Id, ProtoUserTarget)]

    def selectProg(pid: Program.Id): Query0[(UserTarget.Id, ProtoUserTarget)] =
      sql"""
        SELECT id,
               target_id,
               user_target_type,
               observation_index
          FROM user_target
         WHERE program_id = $pid
      """.query[(UserTarget.Id, ProtoUserTarget)]

  }
}
