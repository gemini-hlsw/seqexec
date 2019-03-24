// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

/**
 * A Gem user, parameterized on the type of granted program roles, typically
 * [[gem.enum.ProgramRole ProgramRole]] for a fully specified user, or `Nothing` for a user with
 * unstated grants. Permissions are granted based on a combination of granted roles, staff status,
 * and rootness.
 * @group Application Model
 */
final case class User[A](
  id:        User.Id,
  firstName: String,
  lastName:  String,
  email:     String,
  isStaff:   Boolean,
  roles:     Map[Program.Id, Set[A]]
)

object User {
  type Id = String // TODO
  object Id {
    /** Id of the root user, who always exists. This is a system invariant. */
    val Root: User.Id = "root"
  }
}
