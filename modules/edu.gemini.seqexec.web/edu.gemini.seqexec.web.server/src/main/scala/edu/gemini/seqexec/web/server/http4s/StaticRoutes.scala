package edu.gemini.seqexec.web.server.http4s

import java.io.File

import org.http4s.dsl._
import org.http4s.{HttpService, Request, Response, StaticFile}

import scalaz.concurrent.Task

object StaticRoutes {
  // Get a resource from a local file, useful for development
  def localResource(base: String, path: String, req: Request):Option[Response] = StaticFile.fromFile(new File(base, path), Some(req))
  def localResource(path: String, req: Request):Option[Response] = StaticFile.fromResource(path, Some(req))
  // Get a resource from a local file, used in production
  def embeddedResource(path: String, req: Request):Option[Response] = {
    val url = Option(getClass.getResource(path))
    url.flatMap(StaticFile.fromURL(_, Some(req)))
  }

  implicit class ReqOps(req: Request) {
    def endsWith(exts: String*): Boolean = exts.exists(req.pathInfo.endsWith)

    def serve(path: String = req.pathInfo) = {
      // To find scala.js generated files we need to go into the dir below, hopefully this can be improved
      localResource(path, req).orElse(embeddedResource(path, req))
        .map(Task.now)
        .getOrElse(NotFound())
    }
  }

  val service = HttpService {
    case req if req.pathInfo == "/"                          => req.serve("/index.html")
    case req if req.endsWith(".html", ".js", ".map", ".css") => req.serve()
  }
}