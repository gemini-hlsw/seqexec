// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import cats.effect.IO
import cats.implicits._
import org.http4s.{EntityDecoder, HttpRoutes}
import org.http4s.dsl.io._
import org.http4s.server.middleware.GZip
import org.http4s.circe.jsonOf
import org.log4s.getLogger
import seqexec.server.tcs.GuideConfigDb
import seqexec.server.tcs.GuideConfigDb._



class GuideConfigDbRoutes(db: GuideConfigDb[IO]) {
  // Logger for guide configuration updates
  private val Log = getLogger

  implicit val decoder: EntityDecoder[IO, GuideConfig] = jsonOf

  val publicService: HttpRoutes[IO] = GZip { HttpRoutes.of {
    case req @ POST -> Root =>
      req.decode[GuideConfig] { guideConfig =>
        db.set(guideConfig) *>
          IO(Log.info(s"Received guide configuration $guideConfig")) *>
          Ok("")
      }
  } }

  def service: HttpRoutes[IO] = publicService

}
