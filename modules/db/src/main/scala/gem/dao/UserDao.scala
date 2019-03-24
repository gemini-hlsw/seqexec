// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.dao.meta._
import gem.enum.ProgramRole
import cats.data._, cats.implicits._

import doobie._, doobie.implicits._

object UserDao {
  import EnumeratedMeta._
  import ProgramIdMeta._

  /**
   * Select the root user, for system processes. Always succceds if the system is consistent;
   * raises an exception if the root user is missing.
   * @group Queries
   */
  val selectRootUser: ConnectionIO[User[ProgramRole]] =
    OptionT(selectUser(User.Id.Root)).getOrElse(sys.error("No root user. Cannot continue."))

  // Helper method to add roles to a selected user, if any.
  private def selectUserImpl(q: Query0[User[Nothing]]): ConnectionIO[Option[User[ProgramRole]]] = {
    for {
      u <- OptionT(q.option)
      r <- OptionT(selectRoles(u.id).map(Option(_)))
    } yield u.copy(roles = r)
  } .value

  /**
   * Select the given user, if any, with roles.
   * @group Queries
   */
  def selectUser(uid: User.Id): ConnectionIO[Option[User[ProgramRole]]] =
    selectUserImpl(Statements.selectUser(uid))

  /**
   * Select the given user, if any, with roles, if the id and password match a known user.
   * @group Queries
   */
  def selectUserʹ(uid: User.Id, password: String): ConnectionIO[Option[User[ProgramRole]]] =
    selectUserImpl(Statements.selectUserʹ(uid, password))

  /**
   * Select the map of roles associated with the given user.
   * @group Queries
   */
  def selectRoles(id: User.Id): ConnectionIO[Map[Program.Id, Set[ProgramRole]]] =
    Statements.selectRoles(id).to[List].map(_.foldMap { case (k, v) => Map((k -> Set(v))) })

  /**
   * Insert or update a program role.
   * @group Updates
   */
  def setRole(id: User.Id, pid: Program.Id, role: ProgramRole): ConnectionIO[Unit] =
    Statements.setRole(id, pid, role).run.void

  /**
   * Drop a program role, if it exists, returning true if a role was dropped.
   * @group Updates
   */
  def unsetRole(id: User.Id, pid: Program.Id, role: ProgramRole): ConnectionIO[Boolean] =
    Statements.unsetRole(id, pid, role).run.map(_ > 0)

  /**
   * Insert a user.
   * @group Updates
   */
  def insertUser(user: User[ProgramRole], password: String): ConnectionIO[Unit] =
    Statements.insertUserFlat(user, password).run *>
    user.roles.toList.traverse { case (p, rs) =>
      rs.toList.traverse { r =>
        Statements.setRole(user.id, p, r).run
      }
    } .void

  /**
   * Attempts to change the specified user's password, yielding success. A cause of failure (user
   * not found, old password incorrect) is not given.
   * @group Updates
   */
  def changePassword(uid: User.Id, oldPassword: String, newPassword: String): ConnectionIO[Boolean] =
    Statements.changePassword(uid, oldPassword, newPassword).run.map(_ === 1)

  object Statements {

    private def andHash(password: String): Fragment =
      fr"AND hash = crypt($password, hash)"

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
      selectUserImpl(id, andHash(password))

    def selectRoles(id: User.Id): Query0[(Program.Id, ProgramRole)] =
      sql"""
        SELECT program_id, program_role
        FROM gem_user_program
        WHERE user_id = $id
      """.query

    def changePassword(uid: User.Id, oldPassword: String, newPassword: String): Update0 =
      (fr"""
        UPDATE gem_user
        SET    hash = crypt($newPassword, gen_salt('bf', 10))
        WHERE  id   = ${uid}
      """ ++ andHash(oldPassword)).update

    def setRole(uid: User.Id, pid: Program.Id, role: ProgramRole): Update0 =
      sql"""
        INSERT INTO gem_user_program (user_id, program_id, program_role)
        VALUES ($uid, $pid, $role)
        ON CONFLICT (user_id, program_id, program_role) DO UPDATE
          SET program_role = $role
      """.update

    def unsetRole(uid: User.Id, pid: Program.Id, role: ProgramRole): Update0 =
      sql"""
        DELETE FROM gem_user_program
        WHERE  user_id      = $uid
          AND  program_id   = $pid
          AND  program_role = $role
      """.update

    def insertUserFlat(u: User[_], password: String): Update0 =
      sql"""
        INSERT INTO gem_user (id, first, last, hash, email, staff)
        VALUES (
          ${u.id},
          ${u.firstName},
          ${u.lastName},
          crypt($password, gen_salt('bf', 10)),
          ${u.email},
          ${u.isStaff}
        )
      """.update

  }

}
