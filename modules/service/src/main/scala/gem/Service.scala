package gem

import doobie.imports._

import gem.dao._
import gem.enum._

import scalaz._, Scalaz._
import scalaz.concurrent.Task

case class Service[M[_]](xa: Transactor[M], log: Log[M], user: User[ProgramRole]) {

  /**
   * Return a list of programs whose name or id contains the given substring (case-insensitive), up
   * to a provided maximum length. The use case is an "Open" dialog.
   */
  def queryProgramsByName(substr: String, max: Int): M[List[Program[Nothing]]] =
    log.log(s"""queryProgramsByName("$substr", $max)""") {
      ProgramDao.selectBySubstring(s"%$substr%", max).transact(xa)
    }

  /**
   * Return a program and shallow observations. The use case is an overview page showing the
   * program, allowing the user to navigate to a selected observation.
   */
  def getProgram(id: Program.Id): M[Program[Observation[Nothing]]] =
    log.log(s"getProgram($id)") {
      ???
    }

  /**
   * Fetch an observation with full sequence information. The use case is initial population of a
   * sequence editor.
   */
  def getObservation(id: Observation.Id): M[Observation[Step[_ <: Instrument]]] =
    log.log(s"getObservation($id)") {
      ???
    }

}

object Service {

  def forTesting(uname: String): Service[Task] = {
    val xa = DriverManagerTransactor[Task]("org.postgresql.Driver","jdbc:postgresql:gem","postgres","")
    val io = for {
      user <- UserDao.selectWithRoles(uname).transact(xa)
      log  <- Log.newLog[Task]("Testing", xa)
    } yield Service(xa, log, user)
    io.unsafePerformSync
  }

}
