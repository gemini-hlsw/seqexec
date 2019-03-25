// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package ocs2

import gem.ocs2.Decoders._

import gem.ocs2.pio.{ PioDecoder, PioError }
import gem.ocs2.pio.PioError._

import doobie._
import doobie.implicits._
import cats.effect._
import cats.implicits._
import org.http4s.client._
import org.http4s.client.asynchttpclient.AsyncHttpClient
import org.http4s.scalaxml.xml

import java.net.URLEncoder

import scala.xml.Elem


/** ODB observation/program fetch client.
  */
object OdbClient {

  /** Fetches an observation from an ODB. */
  def fetchObservation[M[_]: ConcurrentEffect](
    host: String,
    id:   Observation.Id
  ): M[Either[String, (Observation, List[Dataset])]] =
    fetch[Observation, M](host, id.format)

  /** Fetches a program from the ODB. */
  def fetchProgram[M[_]: ConcurrentEffect](
    host: String,
    id:   Program.Id
  ): M[Either[String, (Program, List[Dataset])]] =
    fetch[Program, M](host, Program.Id.fromString.reverseGet(id))

  /** Fetches an observation from the ODB and stores it in the database. */
  def importObservation[M[_]: ConcurrentEffect](
    host: String,
    id:   Observation.Id,
    xa:   Transactor[M]
  ): M[Either[String, Unit]] =
    fetchObservation(host, id).flatMap { _.traverse { case (o, ds) =>
      Importer.importObservation(id, o, ds).transact(xa)
    }}

  /** Fetches a program from the ODB and stores it in the database. */
  def importProgram[M[_]: ConcurrentEffect](
    host: String,
    id:   Program.Id,
    xa:   Transactor[M]
  ): M[Either[String, Unit]] =
    fetchProgram(host, id).flatMap { _.traverse { case (p, ds) =>
      Importer.importProgram(p, ds).transact(xa)
    }}

  private def fetchServiceUrl(host: String): String =
    s"http://$host:8442/ocs3/fetch"

  private def uri(host: String, id: String): String =
    s"${fetchServiceUrl(host)}/${URLEncoder.encode(id, "UTF-8")}"

  private def errorMessage(e: PioError): String =
    e match {
      case MissingKey(name)            => s"missing '$name'"
      case ParseError(value, dataType) => s"could not parse '$value' as '$dataType'"
    }

  private def fetch[A: PioDecoder, M[_]: ConcurrentEffect](
    host: String,
    id:   String
  ): M[Either[String, (A, List[Dataset])]] = {

    val client: Resource[M, Client[M]] =
      AsyncHttpClient.resource[M]()

    client.use { c =>
      c.expect[Elem](uri(host, id))
        .map(PioDecoder[(A, List[Dataset])].decode(_).leftMap(errorMessage))
        .attempt
        .map(_.leftMap(ex => s"Problem fetching '$id': ${ex.getMessage}").flatten)
    }

  }

}
