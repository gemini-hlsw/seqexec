// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.enum.ProgramRole
import scalaz._, Scalaz._

import doobie.imports._

object UserDao {

  /**
   * Select the root user, for system processes. Always succceds if the system is consistent;
   * raises an exception if the root user is missing.
   * @group Queries
   */
  val selectRoot: ConnectionIO[User[ProgramRole]] =
    OptionT(selectUser(User.Id.Root)).getOrElse(sys.error("No root user. Cannot continue."))

  // Helper method to add roles to a selected user, if any.
  private def selectUserImpl(q: Query0[User[Nothing]]): ConnectionIO[Option[User[ProgramRole]]] = {
    for {
      u <- OptionT(q.option)
      r <- selectRoles(u.id).liftM[OptionT]
    } yield u.copy(roles = r)
  } .run

  /**
   * Select the given user, with roles.
   * @group Queries
   */
  def selectUser(uid: User.Id): ConnectionIO[Option[User[ProgramRole]]] =
    selectUserImpl(Statements.selectUser(uid))

  /**
   * Select the given user, with roles, if the id and password match a known user.
   * @group Queries
   */
  def selectUserʹ(uid: User.Id, password: String): ConnectionIO[Option[User[ProgramRole]]] =
    selectUserImpl(Statements.selectUserʹ(uid, password))

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
    (Statements.selectUser(id).unique |@| selectRoles(id))((u, r) => u.copy(roles = r))

  /**
   * Attempts to change the specified user's password, yielding success. A cause of failure (user
   * not found, old password incorrect) is not given.
   * @group Updates
   */
  def changePassword(uid: User.Id, oldPassword: String, newPassword: String): ConnectionIO[Boolean] =
    Statements.changePassword(uid, oldPassword, newPassword).run.map(_ === 1)

  object Statements {

    private def selectUserImpl(id: String, and: Fragment): Query0[User[Nothing]] =
      (fr"""
        SELECT id, first, last, email, staff
        FROM gem_user
        WHERE id = $id
       """ ++ and).query[(String, String, String, String, Boolean)].map {
        case (i, f, l, e, s) => User(i, f, l, e, s, Map.empty)
      }

    def selectUser(id: String): Query0[User[Nothing]] =
      selectUserImpl(id, Fragment.empty)

    def selectUserʹ(id: String, password: String): Query0[User[Nothing]] =
      selectUserImpl(id, fr"AND md5 = md5($password)")

    def selectRoles(id: User.Id): Query0[(Program.Id, ProgramRole)] =
      sql"""
        SELECT program_id, program_role
        FROM gem_user_program
        WHERE user_id = $id
      """.query

    def changePassword(uid: User.Id, oldPassword: String, newPassword: String): Update0 =
      sql"""
        UPDATE gem_user
        SET    md5 = md5($newPassword)
        WHERE  id  = ${uid}
        AND    md5 = md5($oldPassword)
      """.update

  }

}
