package edu.gemini.seqexec.web.server.play

import controllers.Assets
import play.api._
import play.api.mvc._

import java.io._

/**
 * Controller that serves static resources from file when possible, or defaults to the classpath
 * It can be used in development mode if you want to serve static assets that shouldn't be part of the build process.
 *
 * Not that this controller is not intended to be used in production mode and can lead to security issues.
 * Therefore it is automatically disabled in production mode.
 *
 * All assets are served with max-age=3600 cache directive.
  *
  * It combines the ExternalAssets and Assets controllers
 *
 */
class CustomAssets (environment: Environment) extends Controller {

  val AbsolutePath = """^(/|[a-zA-Z]:\\).*""".r

  /**
   * Generates an `Action` that serves a static resource from an external folder
   *
   * @param rootPath the root folder for searching the static resource files such as `"src/main/resources"`
   * @param file the file part extracted from the URL
   */
  def at(rootPath: String, file: String, classPathRoot: String): Action[AnyContent] =
    environment.mode match {
      case Mode.Prod => Assets.at(classPathRoot, file)
      case _         =>
        val fileToServe = rootPath match {
          case AbsolutePath(_) => new File(rootPath, file)
          case _               => new File(environment.getFile(rootPath), file)
        }
        // Try first a file (To read from the file system), if that fails go to the classpath
        if (fileToServe.exists) {
          Action {
            Ok.sendFile(fileToServe, inline = true).withHeaders(CACHE_CONTROL -> "max-age=3600")
          }
        } else {
          Assets.at(classPathRoot, file)
        }
    }

}

