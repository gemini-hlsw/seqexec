package gem

import doobie.imports._

import gem.dao._
import gem.enum._

import scalaz._, Scalaz._
import scalaz.concurrent.Task

final class Service[M[_]] private (private val xa: Transactor[M], val log: Log[M], val user: User[ProgramRole]) {

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
    def user[M[_]]: Service[M] @> User[ProgramRole] = Lens.lensu((a, b) => new Service(a.xa, a.log, b), _.user)
  }

  def apply[M[_]](xa: Transactor[M], log: Log[M], user: User[ProgramRole]): Service[M] =
    new Service(xa, log, user)

  /**
   * Construct a program that verifies a user's id and password and returns a `Service`.
   */
  def tryLogin[M[_]: Monad: Catchable: Capture](
    user: User.Id, pass: String, xa: Transactor[M], txa: Transactor[Task]
  ): M[Option[Service[M]]] =
    xa.trans(UserDao.selectWithRoles(user, pass)).flatMap {
      case None    => Option.empty[Service[M]].point[M]
      case Some(u) => Log.newLog[M](s"session:$u.name", txa).map(l => Some(Service[M](xa, l, u)))
    }

}
