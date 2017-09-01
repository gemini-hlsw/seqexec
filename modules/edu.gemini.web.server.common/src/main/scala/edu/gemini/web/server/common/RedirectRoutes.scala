// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.web.server.common

import org.http4s.dsl._
import org.http4s.{HttpService, Uri}

class RedirectToHttpsRoutes(toPort: Int, externalName: String) {
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  private val baseUri = Uri.fromString(s"https://$externalName:$toPort").getOrElse(uri("/"))

  val service: HttpService = HttpService {
    case request => MovedPermanently(baseUri.withPath(request.uri.path))
  }
}
