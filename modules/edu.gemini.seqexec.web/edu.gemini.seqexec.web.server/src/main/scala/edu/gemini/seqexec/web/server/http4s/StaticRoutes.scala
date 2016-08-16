package edu.gemini.seqexec.web.server.http4s

import edu.gemini.seqexec.web.server.OcsBuildInfo
import org.http4s.MediaType._
import org.http4s.dsl._
import org.http4s.headers.{`Cache-Control`, `Content-Type`}
import org.http4s.CacheDirective._
import org.http4s.server.middleware.GZip
import org.http4s.{Charset, Request, Response, StaticFile}
import org.http4s.HttpService
import org.http4s.util.NonEmptyList

import scalaz.concurrent.Task
import scala.concurrent.duration._

class StaticRoutes(devMode: Boolean) {
  val oneYear = 365 * 24 * 60 * 60

  val cacheHeaders = if (devMode) List(`Cache-Control`(NonEmptyList(`no-cache`()))) else List(`Cache-Control`(NonEmptyList(`max-age`(oneYear.seconds))))

  val index = {
    val style = """
                  |   @media screen and (-webkit-min-device-pixel-ratio:0) {
                  |        select,
                  |        textarea,
                  |        input {
                  |          font-size: 16px !important;
                  |        }
                  |      }
                  |"""
    val deps = if (devMode) "edu_gemini_seqexec_web_client-jsdeps.js" else s"edu_gemini_seqexec_web_client-jsdeps.min.${OcsBuildInfo.builtAtMillis}.js"
    val seqexecScript = if (devMode) s"seqexec.js" else s"seqexec-opt.${OcsBuildInfo.builtAtMillis}.js"
    val xml = <html lang="en">
      <head>
        <meta charset="utf-8"/>
        <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
        <meta name="description" content="Seqexec"/>
        <meta name="author" content="Gemini Software Group"/>
        <link rel="icon" href={s"images/launcher.${OcsBuildInfo.builtAtMillis}.ico"}/>

        <!-- Add to homescreen for Safari on iOS -->
        <meta name="apple-mobile-web-app-capable" content="yes"/>
        <meta name="apple-mobile-web-app-status-bar-style" content="black"/>
        <meta name="apple-mobile-web-app-title" content="Seqexec"/>
        <link rel="apple-touch-icon-precomposed" href={s"images/launcher.${OcsBuildInfo.builtAtMillis}.png"}/>

        <title>Seqexec</title>

        <link rel="stylesheet" href={s"css/semantic.${OcsBuildInfo.builtAtMillis}.css"}/>
        <style>{style.stripMargin}</style>
      </head>

      <body>

        <div id="content">
        </div>

        <script src={deps}></script>
        <script src={seqexecScript}></script>
        <script type="text/javascript">
          edu.gemini.seqexec.web.client.SeqexecApp().main();
        </script>
      </body>
    </html>
    s"<!DOCTYPE html>$xml"
  }

  val indexResponse =
    Ok(index).withContentType(Some(`Content-Type`(`text/html`, Charset.`UTF-8`))).putHeaders(`Cache-Control`(NonEmptyList(`no-cache`())))

  // Get a resource from a local file, useful for development
  def localResource(path: String, req: Request):Option[Response] =
    StaticFile.fromResource(path, Some(req)).map(_.putHeaders())

  // Get a resource from a local file, used in production
  def embeddedResource(path: String, req: Request):Option[Response] = {
    val url = Option(getClass.getResource(path))
    url.flatMap(StaticFile.fromURL(_, Some(req)))
  }

  implicit class ReqOps(req: Request) {
    val timestampRegex = s"(.*)\\.${OcsBuildInfo.builtAtMillis}\\.(.*)".r

    /**
      * If a request contains the timestamp remove it to find the original file name
      */
    def removeTimestamp(path: String): String =
      path match {
        case timestampRegex(b, e) => s"$b.$e"
        case xs                   => xs
      }

    def endsWith(exts: String*): Boolean = exts.exists(req.pathInfo.endsWith)

    def serve(path: String = req.pathInfo): Task[Response] = {
      // To find scala.js generated files we need to go into the dir below, hopefully this can be improved
      localResource(removeTimestamp(path), req).orElse(embeddedResource(removeTimestamp(path), req))
        .map(_.putHeaders(cacheHeaders: _*))
        .map(Task.now)
        .getOrElse(NotFound())
    }
  }

  val supportedExtension = List(".html", ".js", ".map", ".css", ".png", ".woff", ".woff2", ".ttf", ".mp3", ".ico")

  val service = GZip { HttpService {
    case req if req.pathInfo == "/"                  => indexResponse
    case req if req.pathInfo == "/cli" && devMode    => req.serve("/cli-dev.html")
    case req if req.pathInfo == "/cli"               => req.serve("/cli.html")
    case req if req.endsWith(supportedExtension: _*) => req.serve()
  }}
}
