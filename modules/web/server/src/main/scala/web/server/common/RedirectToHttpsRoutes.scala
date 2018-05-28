// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.server.common

import cats.effect.IO
import org.http4s.dsl.io._
import org.http4s.headers.Location
import org.http4s.{HttpService, Uri}

class RedirectToHttpsRoutes(toPort: Int, externalName: String) {
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  val baseUri: Uri = Uri.fromString(s"https://$externalName:$toPort").getOrElse(uri("/"))

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  val service: HttpService[IO] = HttpService {
    case request =>
      MovedPermanently(Location(baseUri.withPath(request.uri.path)))
  }
}
