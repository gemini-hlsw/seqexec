// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.dao.meta._
import gem.enum.UserTargetType

import cats.implicits._
import doobie._, doobie.implicits._


object UserTargetDao {

  // A target ID and the corresponding user target type.  We use the id to
  // get the actual target.
  final case class ProtoUserTarget(targetId: Int, targetType: UserTargetType) {

    val toUserTarget: ConnectionIO[Option[UserTarget]] =
      TargetDao.select(targetId).map { _.map(UserTarget(_, targetType)) }
  }

  import EnumeratedMeta._
  import ObservationIdMeta._

  def insert(oid: Observation.Id, userTarget: UserTarget): ConnectionIO[Int] =
    for {
      tid <- TargetDao.insert(userTarget.target)
      uid <- Statements.insert(tid, userTarget.targetType, oid).withUniqueGeneratedKeys[Int]("id")
    } yield uid

  def select(id: Int): ConnectionIO[Option[UserTarget]] =
    for {
      oput <- Statements.select(id).option
      out  <- oput.fold(Option.empty[UserTarget].pure[ConnectionIO]) { _.toUserTarget }
    } yield out

  def selectAll(oid: Observation.Id): ConnectionIO[List[(Int, UserTarget)]] =
    for {
      puts <- Statements.selectAll(oid).list                     // List[(Int, ProtoUserTarget)]
      ots  <- puts.map(_._2.targetId).traverse(TargetDao.select) // List[Option[Target]]
    } yield puts.zip(ots).flatMap { case ((id, put), ot) =>
      ot.map(t => id -> UserTarget(t, put.targetType)).toList
    }


  object Statements {

    def insert(targetId: Int, targetType: UserTargetType, oid: Observation.Id): Update0 =
      sql"""
        INSERT INTO user_target (
          target_id,
          user_target_type,
          observation_id
        ) VALUES (
          $targetId,
          $targetType,
          $oid
        )
      """.update

    def select(id: Int): Query0[ProtoUserTarget] =
      sql"""
        SELECT target_id,
               user_target_type
          FROM user_target
         WHERE id = $id
      """.query[ProtoUserTarget]

    def selectAll(oid: Observation.Id): Query0[(Int, ProtoUserTarget)] =
      sql"""
        SELECT id,
               target_id,
               user_target_type
          FROM user_target
         WHERE observation_id = $oid
      """.query[(Int, ProtoUserTarget)]
  }
}
