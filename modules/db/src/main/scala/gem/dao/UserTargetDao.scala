// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.dao.meta._
import gem.enum.UserTargetType
import gem.math.Index
import gem.syntax.treesetcompanion._

import cats.implicits._
import doobie._, doobie.implicits._

import scala.collection.immutable.{ TreeMap, TreeSet }

// A target ID and the corresponding user target type.  We use the id to get the
// actual target.
final case class ProtoUserTarget(
  id:         UserTarget.Id,
  targetId:   Target.Id,
  targetType: UserTargetType,
  obsIndex:   Index
) extends ProtoTargetWrapper[UserTarget.Id, UserTarget] {

  override def wrap(t: Target): UserTarget =
    UserTarget(t, targetType)

}

object UserTargetDao extends TargetWrapperDao[UserTarget.Id, UserTarget, ProtoUserTarget] {

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
    selectOne(Statements.select(id))

  /**
   * Selects all `UserTarget`s for an observation.
   */
  def selectObs(oid: Observation.Id): ConnectionIO[TreeSet[UserTarget]] =
    selectAll(Statements.selectObs(oid)).map(lst => TreeSet.fromList(wrapAll(lst)))

  /**
   * Selects all `UserTarget`s for an observation paired with the `UserTarget`
   * id itself.
   */
  def selectObsWithId(oid: Observation.Id): ConnectionIO[TreeMap[UserTarget.Id, UserTarget]] =
    selectAll(Statements.selectObs(oid)).map(groupById)

  /**
   * Selects all `UserTarget`s for a program.
   */
  def selectProg(pid: Program.Id): ConnectionIO[Map[Index, TreeSet[UserTarget]]] =
    selectAll(Statements.selectProg(pid))
      .map(groupAndMap(_.obsIndex, lst => TreeSet.fromList(wrapAll(lst))))

  /**
   * Selects all `UserTarget`s for a program paired with the `UserTarget` id
   * itself.
   */
  def selectProgWithId(pid: Program.Id): ConnectionIO[TreeMap[Index, TreeMap[UserTarget.Id, UserTarget]]] =
    selectAll(Statements.selectProg(pid))
      .map(groupAndMap(_.obsIndex, groupById))


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
        SELECT id,
               target_id,
               user_target_type,
               observation_index
          FROM user_target
         WHERE id = $id
      """.query[ProtoUserTarget]

    def selectObs(oid: Observation.Id): Query0[ProtoUserTarget] =
      sql"""
        SELECT id,
               target_id,
               user_target_type,
               observation_index
          FROM user_target
         WHERE program_id = ${oid.pid} AND observation_index = ${oid.index}
      """.query[ProtoUserTarget]

    def selectProg(pid: Program.Id): Query0[ProtoUserTarget] =
      sql"""
        SELECT id,
               target_id,
               user_target_type,
               observation_index
          FROM user_target
         WHERE program_id = $pid
      """.query[ProtoUserTarget]

  }
}
