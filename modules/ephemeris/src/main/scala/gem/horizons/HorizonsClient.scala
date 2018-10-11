// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.enum.Site

import cats.ApplicativeError
import cats.data.Reader
import cats.effect.{ IO, ContextShift, Resource }
import cats.implicits._

import org.http4s._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder

import fs2.Stream
//import fs2.async.mutable.Semaphore
import fs2.text.utf8Decode

import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant
import java.time.ZoneOffset.UTC
import java.time.format.DateTimeFormatter
import java.util.Locale.US

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/** A client for interacting with the JPL horizons service.
  */
object HorizonsClient {

  private implicit val contextShift: ContextShift[IO] =
    IO.contextShift(scala.concurrent.ExecutionContext.global)

  /** A global mutex for limiting concurrent connections to Horizons. */
//  private val mutex: Semaphore[IO] =
//    Semaphore[IO](1L)(implicitly, scala.concurrent.ExecutionContext.global).unsafeRunSync // note

  /** A stream that emits a single client that will be shut down automatically. An global mutex
    * ensures that only one such stream exists at a time.
    */
  val client: Resource[IO, Client[IO]] = {
    // By using the singleton type here we prove that the mutex is a constant value. This guards
    // against someone accidentally refactoring it into a method.
//    val mutexʹ: mutex.type = mutex
    val client = BlazeClientBuilder[IO](ExecutionContext.global).withResponseHeaderTimeout(20.seconds)
//    Stream.bracket(mutexʹ.decrement)(_ => client, _ => mutexʹ.increment)
    client.resource
  }

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
    urlString.map(Uri.fromString).map(ApplicativeError[IO, Throwable].fromEither)

  /** Creates an http4s Request representing the request. */
  val request: ParamReader[IO[Request[IO]]] =
    uri.map { _.map(Request(Method.GET, _)) }

  /** Creates a `Stream` of results from the horizons server when executed. */
  val stream: ParamReader[Stream[IO, String]] =
    request.map { r =>
      for {
        c <- Stream.resource(client)
        a <- Stream.eval(r).flatMap(c.stream)
        s <- a.body.through(utf8Decode)
      } yield s
    }

  /** Retrieves all the horizons server outout into a single String. */
  val fetch: ParamReader[IO[String]] =
    request.map { r =>
      client.use { c =>
        c.expect[String](r)
      }
    }

}

