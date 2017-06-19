/*
 * Copyright (c) 2017, Association of Universities for Research in Astronomy, Inc. (AURA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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
    Statements.select(id).unique

  def selectWithRoles(id: String, pass: String): ConnectionIO[Option[User[ProgramRole]]] =
    Statements.selectWithRoles(id, pass)
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

  def changePassword(uid: User.Id, oldPassword: String, newPassword: String): ConnectionIO[Boolean] =
    Statements.changePassword(uid, oldPassword, newPassword).run.map(_ == 1)

  object Statements {

    def select(id: String): Query0[User[Nothing]] =
      sql"""
        SELECT id, first, last, email, staff
        FROM gem_user
        WHERE id = $id
      """.query[(String, String, String, String, Boolean)]
         .map { case (i, f, l, e, s) =>
           User(i, f, l, e, s, Map.empty)
         }

    def selectWithRoles(id: String, pass: String): Query0[(String, String, String, String, Boolean, Option[Program.Id], Option[ProgramRole])] =
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

    def changePassword(uid: User.Id, oldPassword: String, newPassword: String): Update0 =
      sql"""
        UPDATE gem_user
        SET    md5 = md5($newPassword)
        WHERE  id  = ${uid}
        AND    md5 = md5($oldPassword)
      """.update

  }

}
