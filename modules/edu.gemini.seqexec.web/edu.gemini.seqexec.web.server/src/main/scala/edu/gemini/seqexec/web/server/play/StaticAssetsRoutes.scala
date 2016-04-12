package edu.gemini.seqexec.web.server.play

import play.api.Environment
import play.api.routing.Router._
import play.api.routing.sird._

/**
  * Routes for static assets
  */
class StaticAssetsRoutes(environment: Environment) {
  val customAssets = new CustomAssets(environment)

  val routes: Routes = {
    case GET(p"/") =>
      // Index
      customAssets.at("./src/main/resources", "index.html", "/")
    case GET(p"/$f*") =>
      // Static files,
      customAssets.at("src/main/resources", f, "/")
  }
}
