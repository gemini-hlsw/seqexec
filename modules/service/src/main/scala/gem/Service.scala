package gem

import doobie.imports._

import gem.dao._
import gem.enum.Instrument

import scalaz._, Scalaz._
import scalaz.concurrent.Task

case class Service[M[_]](xa: Transactor[M], log: Log[M]) {

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

  def shutdown(ms: Long): M[Unit] =
    log.shutdown(ms)

}

object Service {

  def forTesting: Service[Task] = {
    val xa = DriverManagerTransactor[Task]("org.postgresql.Driver","jdbc:postgresql:gem","postgres","")
    Log.newLog[Task]("Testing", xa).map(Service(xa, _)).unsafePerformSync
  }

}
