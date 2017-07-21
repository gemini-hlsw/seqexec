// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.enum.ProgramRole
import scalaz._, Scalaz._

import doobie.imports._

object UserDao {

  /**
   * Select the super-user, for system processes.
   * @group Queries
   */
  val selectRoot: ConnectionIO[User[Nothing]] =
    selectUser("root")

  /**
   * Select the given user, with roles, if the id and password match a known user.
   * @group Queries
   */
  def tryLogin(uid: User.Id, password: String): ConnectionIO[Option[User[ProgramRole]]] =
    (Statements.countUsers(uid, password).unique.liftM[OptionT].filter(_ == 1) *>
     selectUserWithRoles(uid).liftM[OptionT]).run

  /**
   * Select the given user, without roles. Raises an exception if the user is not found.
   * @group Queries
   */
  def selectUser(id: User.Id): ConnectionIO[User[Nothing]] =
    Statements.selectUser(id).unique

  /**
   * Select the map of roles associated with the given user.
   * @group Queries
   */
  def selectRoles(id: User.Id): ConnectionIO[Map[Program.Id, Set[ProgramRole]]] =
    Statements.selectRoles(id).list.map(_.foldMap { case (k, v) => Map((k -> Set(v))) })

  /**
   * Select the given user, with roles. Raises an exception if the user is not found.
   * @group Queries
   */
  def selectUserWithRoles(id: User.Id): ConnectionIO[User[ProgramRole]] =
    (Statements.selectUser(id).unique |@| selectRoles(id))((u, r) => u.copy(allProgramRoles = r))

  /**
   * Attempts to change the specified user's password, yielding success. A cause of failure (user
   * not found, old password incorrect) is not given.
   * @group Updates
   */
  def changePassword(uid: User.Id, oldPassword: String, newPassword: String): ConnectionIO[Boolean] =
    Statements.changePassword(uid, oldPassword, newPassword).run.map(_ === 1)

  object Statements {

    def selectUser(id: String): Query0[User[Nothing]] =
      sql"""
        SELECT id, first, last, email, staff
        FROM gem_user
        WHERE id = $id
      """.query[(String, String, String, String, Boolean)].map {
        case (i, f, l, e, s) => User(i, f, l, e, s, Map.empty)
      }

    def selectRoles(id: User.Id): Query0[(Program.Id, ProgramRole)] =
      sql"""
        SELECT program_id, program_role
        FROM gem_user_program
        WHERE user_id = $id
      """.query

    // count the number of users with the given id and password; useful for checking passwords
    def countUsers(uid: User.Id, password: String): Query0[Int] =
      sql"""
        SELECT count(*)::integer
        FROM   gem_user
        WHERE  id  = ${uid}
        AND    md5 = md5($password)
      """.query[Int]

    def changePassword(uid: User.Id, oldPassword: String, newPassword: String): Update0 =
      sql"""
        UPDATE gem_user
        SET    md5 = md5($newPassword)
        WHERE  id  = ${uid}
        AND    md5 = md5($oldPassword)
      """.update

  }

}
