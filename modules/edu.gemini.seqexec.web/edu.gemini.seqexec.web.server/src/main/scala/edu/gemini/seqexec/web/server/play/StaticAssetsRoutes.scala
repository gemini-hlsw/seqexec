package edu.gemini.seqexec.web.server.play

import play.api.routing.Router._
import play.api.routing.sird._

/**
  * Routes for static assets
  */
object StaticAssetsRoutes {
  val routes: Routes = {
    case GET(p"/") =>
      // Index
      CustomAssets.at("./src/main/resources", "index.html", "/")
    case GET(p"/$f*") =>
      // Static files,
      CustomAssets.at("src/main/resources", f, "/")
  }
}
