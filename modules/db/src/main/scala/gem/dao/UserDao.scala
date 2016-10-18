package gem
package dao

import gem.enum.ProgramRole
import scalaz._, Scalaz._

import doobie.imports._

object UserDao {

  /** Select the super-user, for system processes. */
  def selectRoot: ConnectionIO[User[Nothing]] =
    select("root")

  def select(id: String): ConnectionIO[User[Nothing]] =
    sql"""
      SELECT id, first, last, email, staff
      FROM gem_user
      WHERE id = $id
    """.query[(String, String, String, String, Boolean)]
       .unique.map { case (i, f, l, e, s) =>
         User(i, f, l, e, s, Map.empty)
       }

  def selectWithRoles(id: String, pass: String): ConnectionIO[Option[User[ProgramRole]]] =
    sql"""
      SELECT u.id,
             u.first,
             u.last,
             u.email,
             u.staff,
             g.program_id,
             g.program_role
        FROM gem_user u LEFT OUTER JOIN gem_user_program g ON g.user_id = u.id
        WHERE id = $id
        AND "md5" = md5($pass)
    """
      .query[(String, String, String, String, Boolean, Option[Program.Id], Option[ProgramRole])]
      .list
      .map { rows =>
        rows.headOption.map {
          case (i, f, l, e, s, _, _) =>
            val map = rows.collect {
              case (_, _, _, _, _, Some(pid), Some(role)) => Map(pid -> Set(role))
            }.suml
            User(i, f, l, e, s, map)
        }
      }
}
