package edu.gemini.seqexec.web.server.play

import javax.inject.Inject

import controllers.Assets
import play.api._
import play.api.mvc._

import Play.current

import java.io._

object CustomAssets extends CustomAssets

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
class CustomAssets @Inject() () extends Controller {

  val AbsolutePath = """^(/|[a-zA-Z]:\\).*""".r

  /**
   * Generates an `Action` that serves a static resource from an external folder
   *
   * @param rootPath the root folder for searching the static resource files such as `"src/main/resources"`
   * @param file the file part extracted from the URL
   */
  def at(rootPath: String, file: String, classPathRoot: String): Action[AnyContent] =
    Play.mode match {
      case Mode.Prod => Action {
                          NotFound
                        }
      case _         =>
        val fileToServe = rootPath match {
          case AbsolutePath(_) => new File(rootPath, file)
          case _               => new File(Play.application.getFile(rootPath), file)
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

