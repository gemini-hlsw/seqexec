package edu.gemini.seqexec.web.server.play

//import com.unboundid.ldap.sdk.{LDAPConnection, LDAPException, ResultCode, SimpleBindRequest}
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.server.{ExecutorImpl, SeqexecFailure}
import edu.gemini.seqexec.web.common._
import play.api.routing.Router
import play.api.{BuiltInComponents, Mode}
import play.core.server.{NettyServerComponents, ServerConfig}
import play.api._
import play.api.mvc._
import play.api.routing.sird._
import edu.gemini.seqexec.web.server.model.Conversions._
import upickle.default._

import scalaz._
import Scalaz._

object WebServerLauncher extends App {

  def launch(port: Int):NettyServerComponents = {
    new NettyServerComponents with BuiltInComponents with Controller {
      override lazy val serverConfig = ServerConfig(
        port = Some(port),
        address = "0.0.0.0"
      ).copy(mode = Mode.Dev)

      lazy val router = Router.from {
        case GET(p"/") =>
          // Index
          CustomAssets.at("./src/main/resources", "index.html", "/")
        case GET(p"/api/sequence/$id<.*-[0-9]+>") => Action {
          val obsId = new SPObservationID(id)
          ExecutorImpl.read(obsId) match {
            case \/-(s) => Results.Ok(write(Sequence(obsId.stringValue(), s.toSequenceSteps)))
            case -\/(e) => Results.NotFound(SeqexecFailure.explain(e))
          }
        }
        /*case POST(p"/login") => Action(parse.text) { b =>
          // TODO hide the server name
          // TODO Make it asynchronous
          // TODO Make the connection closeable
          // TODO Use a connection pool
          val c = new LDAPConnection("gs-dc6.gemini.edu", 3268)
          try {
            val u = read[UserCredentials](b.body)
            val bindRequest = new SimpleBindRequest(u.username, u.password)
            c.bind(bindRequest)
            Results.Ok("ok")
          } catch {
            case e:LDAPException if e.getResultCode == ResultCode.INVALID_CREDENTIALS   =>
              Results.Unauthorized("Bad credentials")
            case e:LDAPException =>
              Results.Unauthorized("Bad credentials")
            case e:Exception =>
              Results.InternalServerError(e.getMessage)
          } finally {
            c.close()
          }
        }*/
        case GET(p"/$f*") =>
          // Static files,
          CustomAssets.at("src/main/resources", f, "/")
      }
  }}

  launch(9090).server

}
