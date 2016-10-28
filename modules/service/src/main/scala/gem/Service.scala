package gem

import doobie.imports._

import gem.dao._
import gem.enum._

import scalaz._, Scalaz._
import scalaz.concurrent.Task

final class Service[M[_]] private (private val xa: Transactor[M], val log: Log[M], val user: User[ProgramRole]) {

  /**
   * Return a list of programs whose name or id contains the given substring (case-insensitive), up
   * to a provided maximum length. The use case is an "Open" dialog.
   */
  def queryProgramsByName(substr: String, max: Int): M[List[Program[Nothing]]] =
    log.log(user, s"""queryProgramsByName("$substr", $max)""") {
      ProgramDao.selectBySubstring(s"%$substr%", max).transact(xa)
    }

  /**
   * Return a program and shallow observations. The use case is an overview page showing the
   * program, allowing the user to navigate to a selected observation.
   */
  def getProgram(id: Program.Id): M[Program[Observation[Nothing]]] =
    log.log(user, s"getProgram($id)") {
      ???
    }

  /**
   * Fetch an observation with full sequence information. The use case is initial population of a
   * sequence editor.
   */
  def getObservation(id: Observation.Id): M[Observation[Step[_ <: Instrument]]] =
    log.log(user, s"getObservation($id)") {
      ???
    }

}

object Service {

  def apply[M[_]](xa: Transactor[M], log: Log[M], user: User[ProgramRole]): Service[M] =
    new Service(xa, log, user)

  object L {
    def user[M[_]]: Service[M] @> User[ProgramRole] = Lens.lensu((a, b) => new Service(a.xa, a.log, b), _.user)
  }

}
