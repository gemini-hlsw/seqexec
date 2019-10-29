// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.server.common

import cats.effect.Sync
import org.http4s.dsl._
import org.http4s.headers.Location
import org.http4s.{HttpRoutes, Uri}

class RedirectToHttpsRoutes[F[_]: Sync](toPort: Int, externalName: String) extends Http4sDsl[F] {
  val baseUri: Uri = Uri.fromString(s"https://$externalName:$toPort").getOrElse(Uri.uri("/"))

  val service: HttpRoutes[F] = HttpRoutes.of[F] {
    case request =>
      MovedPermanently(Location(baseUri.withPath(request.uri.path)))
  }
}
