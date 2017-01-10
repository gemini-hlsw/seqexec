package gem.seqimporter

import gem.{Observation, Step}
import gem.config.InstrumentConfig

import org.http4s._
import org.http4s.dsl._
import org.http4s.client.blaze._
import org.http4s.scalaxml.{xml, _}

import scala.xml.{Elem, XML}
import scalaz.concurrent.Task

/** Fetches a sequence from the ODB, translates it into the sequence model and
  * writes it into the database, replacing anything that might have been there
  * already.
  */
object SequenceImporter {

  private val client = PooledHttp1Client()

  private def fetchSequenceXml(oid: Observation.Id): Task[Elem] = {
    // WDBA implements an XML-RPC service for fetching sequences.  We post it
    // the following XML and it returns the matching sequence.
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

  def fetchSequence(oid: Observation.Id): Task[List[Step[(InstrumentConfig, Boolean)]]] =
    fetchSequenceXml(oid).map { SequenceReader.unsafeParse }
}
