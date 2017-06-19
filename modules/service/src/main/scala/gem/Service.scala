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

import doobie.imports._

import gem.dao._
import gem.enum._

import scalaz._, Scalaz._
import scalaz.concurrent.Task

final class Service[M[_]: Monad] private (private val xa: Transactor[M, _], val log: Log[M], val user: User[ProgramRole]) {

  /**
   * Construct a program that yields a list of `Program` whose name or id contains the given
   * substring (case-insensitive), up to a provided maximum length.
   */
  def queryProgramsByName(substr: String, max: Int): M[List[Program[Nothing]]] =
    log.log(user, s"""queryProgramsByName("$substr", $max)""") {
      ProgramDao.selectBySubstring(s"%$substr%", max).transact(xa)
    }

  /**
   * Construct a program that attempts to change the user's password, yielding `true` on success.
   */
  def changePassword(oldPassword: String, newPassword: String): M[Boolean] =
    log.log(user, "changePassword(***, ***)") {
      UserDao.changePassword(user.id, oldPassword, newPassword).transact(xa)
    }

}

object Service {

  object L {
    def user[M[_]: Monad]: Service[M] @> User[ProgramRole] = Lens.lensu((a, b) => new Service(a.xa, a.log, b), _.user)
  }

  def apply[M[_]: Monad](xa: Transactor[M, _], log: Log[M], user: User[ProgramRole]): Service[M] =
    new Service(xa, log, user)

  /**
   * Construct a program that verifies a user's id and password and returns a `Service`.
   */
  def tryLogin[M[_]: Monad: Catchable: Capture](
    user: User.Id, pass: String, xa: Transactor[M,_], txa: Transactor[Task, _]
  ): M[Option[Service[M]]] =
    xa.trans.apply(UserDao.selectWithRoles(user, pass)).flatMap {
      case None    => Option.empty[Service[M]].point[M]
      case Some(u) => Log.newLog[M](s"session:$u.name", txa).map(l => Some(Service[M](xa, l, u)))
    }

}
