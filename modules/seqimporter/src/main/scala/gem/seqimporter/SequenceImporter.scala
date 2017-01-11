package gem.seqimporter

import doobie.imports._
import gem.{Dataset, Location, Log, Observation, Program, Step}
import gem.config.InstrumentConfig
import gem.dao._
import org.http4s._
import org.http4s.dsl._
import org.http4s.client.blaze._
import org.http4s.scalaxml.{xml, _}

import java.time.Instant
import java.util.logging.{Level, Logger}

import scala.xml.{Elem, Node, XML}
import scalaz._
import Scalaz._
import scalaz.concurrent.Task

/** Fetches a sequence from the ODB, translates it into the sequence model and
  * writes it into the database, replacing anything that might have been there
  * already.
  */
object SequenceImporter {

  val lxa = DriverManagerTransactor[Task](
    "org.postgresql.Driver",
    "jdbc:postgresql:gem",
    "postgres",
    ""
  )

  private val client = PooledHttp1Client()

  /** Requests a sequence from the WDBA using XML-RPC, parses the result into
    * sequence XML.
    */
  private def fetchSequenceXml(oid: Observation.Id): Task[Elem] = {
    // WDBA implements an XML-RPC service for fetching sequences.  We post the
    // following XML and it returns the matching sequence.
    val req: Task[Request] =
      Request(Method.POST, Uri.unsafeFromString("http://gsodb.gemini.edu:8442/wdba")).withBody {
        <methodCall>
          <methodName>WDBA_Exec.getSequence</methodName>
          <params><param><value><string>{oid}</string></value></param></params>
        </methodCall>
      }

    // Run the request on the ODB, which returns the sequence XML if all goes
    // well wrapped in XML-RPC elements.
    client.expect[Elem](req).map { elem =>
      // Fish the return value out of the XML-RPC result.
      val xmlTxt = (elem \\ "value").text

      // Drop the preliminary cruft and then parse the sequence XML.  We're
      // assuming that the observation was found in the ODB and all went well
      // or else this will fail with an exception.  We could fish out XML-RPC
      // failure results and return more accurate failure information if needed.
      XML.loadString(xmlTxt.lines.drop(3).mkString("\n"))
    }
  }

  /** Converts the OCS2 sequence XML into the new sequence model.  Each step
    * includes the instrument configuration and its completion state.
    */
  private def parseSequenceXml(oid: Observation.Id, xml: Elem): List[Step[(InstrumentConfig, Boolean)]] = {
    // Extracts the steps in the XML sequence to a simple list of Maps where
    // each Map is all the keys and values that apply to the step.
    def configMaps(xml: Elem): List[ConfigMap] = {
      def extractSystem(configMap: ConfigMap, sys: Node): ConfigMap = {
        val sysName = (sys \ "@name").text

        (configMap /: (sys \ "param")) { (map, param) =>
          val paramName  = (param \ "@name").text
          val paramValue = (param \ "@value").text
          map + (s"$sysName:$paramName" -> paramValue)
        }
      }

      def extractStep(configMap: ConfigMap, step: Node): ConfigMap =
        (configMap /: (step \ "system")) { extractSystem }

      (xml \ "step").toList.scanLeft(EmptyConfigMap) { extractStep }.tail
    }

    configMaps(xml).map(ConfigReader.unsafeStep)
  }

  /** Fetches a Sequence from the ODB using its WDBA service, translating it
    * into the new sequence model in memory.
    */
  def fetchSequence(oid: Observation.Id): Task[List[Step[(InstrumentConfig, Boolean)]]] =
    fetchSequenceXml(oid).map { parseSequenceXml(oid, _) }

  private val configureLogging: Task[Unit] =
    Task.delay(List(
      "edu.gemini.spModel.type.SpTypeUtil"
    ).map(Logger.getLogger).foreach(_.setLevel(Level.OFF)))

  // We need a minimal program/observation in order to insert steps.  The
  // seqexec doesn't actually use these but they have to be there to satisfy
  // database constraints.
  private def createMinimalContext(oid: Observation.Id, s: List[Step[(InstrumentConfig, Boolean)]]): Task[Unit] = {
    val i = s.headOption.map(_.instrument._1.instrument)
    val p = Program[Nothing](oid.pid, "", Nil)
    val o = Observation[Nothing](oid, "", i, Nil)

    (for {
      _ <- ProgramDao.insert(p)
      _ <- ObservationDao.insert(o)
    } yield ()).transact(lxa)
  }

  // Writes the sequence to the database.
  private def writeSequence(oid: Observation.Id, s: List[Step[(InstrumentConfig, Boolean)]]): Task[Unit] = {
    // Create datasets for the complete steps. Fake filename and timestamp.
    // Seqexec doesn't need to read these values anyway.
    val datasets = s.map(_.instrument._2).zipWithIndex.map { case (complete, i) =>
      complete option Dataset(Dataset.Label(oid, i+1), s"unused", Instant.now)
    }

    // Extract a List[Step[InstrumentConfig]], dropping the completion state.
    // Zip the steps with a made up Location that spaces out the steps a bit.
    val steps        = s.map(_.map(_._1))
    val stepsWithLoc = steps.zipWithIndex.map(_.map(i => Location.unsafeMiddle(i * 100)))

    // Write them to the database.
    (for {
      ids <- stepsWithLoc.traverseU { case (s,l) => StepDao.insert(oid, l, s) }
      _   <- ids.zip(datasets).traverseU { case (i, od) =>
               od.fold(0.point[ConnectionIO]) { DatasetDao.insert(i, _) }
             }
    } yield ()).transact(lxa)
  }

  /** Fetches the corresponding sequence from the ODB and writes it into the
    * database.
    *
    * @param oid observation whose sequence should be imported
    */
  def importSequence(oid: Observation.Id): Task[Unit] =
    for {
      u <- UserDao.selectRoot.transact(lxa)
      l <- Log.newLog[Task]("seqimporter", lxa)
      _ <- configureLogging
      s <- l.log(u, s"fetch $oid sequence from ODB" )(fetchSequence(oid))
      _ <- createMinimalContext(oid, s)
      _ <- l.log(u, s"delete existing $oid sequence")(StepDao.delete(oid).transact(lxa))
      _ <- l.log(u, s"write $oid sequence"          )(writeSequence(oid, s))
      _ <- l.shutdown(5 * 1000) // if we're not done soon something is wrong
    } yield ()
}
