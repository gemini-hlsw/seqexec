// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.enum.ProgramRole

import scala.language.implicitConversions

import scalaz._, Scalaz._, scalaz.syntax.Ops

final case class User[A](
  id: User.Id,
  firstName: String,
  lastName:  String,
  email: String,
  isStaff: Boolean,
  allProgramRoles: Map[Program.Id, Set[A]]
)

object User {
  type Id = String // TODO
}

trait UserProgramRoleOps extends Ops[User[ProgramRole]] {

  def programRoles(pid: Program.Id): Set[ProgramRole] =
    self.allProgramRoles.get(pid).orZero

  def hasProgramRole(pid: Program.Id, role: ProgramRole): Boolean =
    programRoles(pid).contains(role)

}

trait ToUserProgramRoleOps {
  implicit def toUserProgramRoleOps(self0: User[ProgramRole]): UserProgramRoleOps =
    new UserProgramRoleOps {
      val self: User[ProgramRole] = self0
    }
}
