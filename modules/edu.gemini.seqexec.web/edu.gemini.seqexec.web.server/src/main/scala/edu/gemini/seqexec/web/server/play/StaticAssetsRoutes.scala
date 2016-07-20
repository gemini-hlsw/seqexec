package edu.gemini.seqexec.web.server.play

import play.api.{Environment, Mode}
import play.api.routing.Router._
import play.api.routing.sird._

/**
  * Routes for static assets
  */
class StaticAssetsRoutes(environment: Environment) {
  val customAssets = new CustomAssets(environment)

  val routes: Routes = {
    case GET(p"/") if environment.mode == Mode.Dev    =>
      // Index
      customAssets.at("./src/main/resources", "index-dev.html", "/")
    case GET(p"/")                                    =>
      // Index
      customAssets.at("./src/main/resources", "index.html", "/")
    case GET(p"/cli") if environment.mode == Mode.Dev =>
      // Index
      customAssets.at("./src/main/resources", "cli-dev.html", "/")
    case GET(p"/cli")                                 =>
      // Index
      customAssets.at("./src/main/resources", "cli.html", "/")
    case GET(p"/$f*")                                 =>
      // Static files,
      customAssets.at("src/main/resources", f, "/")
  }
}
