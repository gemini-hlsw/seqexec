// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.json.syntax.format._
import gem.util.Timestamp
import io.circe.{ Decoder, Encoder, Json }
import io.circe.syntax._
import java.time.{ Duration, Instant }

trait TimeJson {

  // Instant as a record with epoch seconds and nanosecond-of-second
  implicit val DurationEncoder: Encoder[Duration] = d =>
    Json.obj(
      "seconds"     -> d.getSeconds.asJson,
      "nanoseconds" -> d.getNano.asJson
    )
  implicit val DurationDecoder: Decoder[Duration] = c =>
    for {
      ss <- c.downField("seconds")    .as[Long]
      ns <- c.downField("nanoseconds").as[Long]
    } yield Duration.ofSeconds(ss, ns)

  // Instant as a record with epoch seconds and nanosecond-of-second
  implicit val InstantEncoder: Encoder[Instant] = i =>
    Json.obj(
      "seconds"     -> i.getEpochSecond.asJson,
      "nanoseconds" -> i.getNano.asJson
    )
  implicit val InstantDecoder: Decoder[Instant] = c =>
    for {
      ss <- c.downField("seconds")    .as[Long]
      ns <- c.downField("nanoseconds").as[Long]
    } yield Instant.ofEpochSecond(ss, ns)

  implicit val TimestampEncoder: Encoder[Timestamp] = Timestamp.instant.toEncoder
  implicit val TimestampDecoder: Decoder[Timestamp] = Timestamp.instant.toDecoder

}
object time extends TimeJson