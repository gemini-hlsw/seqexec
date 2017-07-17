// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

/**
 * A Gem user, parameterized on the type of owned program roles, typically
 * [[gem.enum.ProgramRole ProgramRole]] for a fully specified user, or `Nothing` for a user with
 * unstated permission information.
 * @group Application Model
 */
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
