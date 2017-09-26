// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.enum.Site

import cats.data.Reader
import cats.effect.IO
import cats.implicits._

import org.http4s._
import org.http4s.client.Client
import org.http4s.client.blaze.PooledHttp1Client

import fs2.Stream
import fs2.text.utf8Decode

import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant
import java.time.ZoneOffset.UTC
import java.time.format.DateTimeFormatter
import java.util.Locale.US

/** A client for interacting with the JPL horizons service.
  */
object HorizonsClient {

  /** Horizons permits only one request at a time from a given host, so all
    * requests should go through this client.
    */
  val client: Client[IO] =
    PooledHttp1Client[IO](maxTotalConnections = 1)

  /** Horizons service URL. */
  val Url: String =
    "https://ssd.jpl.nasa.gov/horizons_batch.cgi"

  /** DateTimeFormatter that can format instants into the string expected by
    * the horizons service: {{{yyyy-MMM-d HH:mm:ss.SSS}}}.
    */
  val DateFormat: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MMM-d HH:mm:ss.SSS", US).withZone(UTC)

  val SharedParams: Map[String, String] =
    Map(
      "batch"       -> "1",
      "CSV_FORMAT"  -> "NO",
      "TABLE_TYPE"  -> "OBSERVER"
    )

  type ParamReader[A] = Reader[Map[String, String], A]

  /** Format coordinates and altitude as expected by horizons.*/
  def formatCoords(s: Site): String =
    f"'${s.longitude.toDoubleDegrees}%1.5f,${s.latitude.toSignedDoubleDegrees}%1.6f,${s.altitude.toDouble/1000.0}%1.3f'"

  /** Format an instant as expected by horizons. */
  def formatInstant(i: Instant): String =
    s"'${DateFormat.format(i)}'"

  /** URL encodes query params. */
  val formatQuery: ParamReader[String] =
    Reader {
      _.toList
       .map { case (k, v) => s"$k=${URLEncoder.encode(v, UTF_8.name)}" }
       .intercalate("&")
    }

  /** Computes the URL string that matches the given parameters. */
  val urlString: ParamReader[String] =
    formatQuery.map { s => s"$Url?$s" }

  /** Creates an http4s Uri representing the request and lifts it into IO,
    * raising an error if there is a problem parsing the request.
    */
  val uri: ParamReader[IO[Uri]] =
    urlString.map(Uri.fromString).map(_.fold(IO.raiseError, IO.pure))

  /** Creates an http4s Request representing the request. */
  val request: ParamReader[IO[Request[IO]]] =
      uri.map { _.map(Request(Method.GET, _)) }

  /** Creates a `Stream` of results from the horizons server when executed. */
  val stream: ParamReader[Stream[IO, String]] =
    request.map { client.streaming(_) { _.body.through(utf8Decode) } }

  /** Retrieves all the horizons server outout into a single String. */
  val fetch: ParamReader[IO[String]] =
    request.map { client.expect[String](_) }
}
