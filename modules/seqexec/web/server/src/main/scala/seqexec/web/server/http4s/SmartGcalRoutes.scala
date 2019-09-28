// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import cats.effect.Sync
import cats.implicits._
import org.http4s._
import org.http4s.dsl.Http4sDsl

/**
  * Rest Endpoints for SmartGcal
  */
class SmartGcalRoutes[F[_]: Sync](cal: SmartGcal) extends Http4sDsl[F] {

  val publicService: HttpRoutes[F] = HttpRoutes.of {
    // This route can be used to manually refresh smart gcal
    // In practice a cronjob calls this route once a day
    case POST -> Root / "refresh" =>
      Sync[F].delay(cal.cal.updateNowInBackground()) *>
        Ok("Smart GCal Refresh started")
  }

  def service: HttpRoutes[F] = publicService
}
