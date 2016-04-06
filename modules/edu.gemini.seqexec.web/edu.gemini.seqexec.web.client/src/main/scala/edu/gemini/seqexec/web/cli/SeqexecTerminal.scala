package edu.gemini.seqexec.web.cli

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import org.querki.jquery.$
import org.scalajs.dom.document
import org.scalajs.dom.ext.Ajax
import JQueryTerminal.{Terminal, _}
import edu.gemini.seqexec.web.common.RegularCommand

import scala.scalajs.js.Any
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import upickle.default._

import scalaz._
import Scalaz._

@JSExport("SeqexecTerminal")
object SeqexecTerminal extends js.JSApp {
  type CommandFunction = (List[String], Terminal) => js.Any

  trait CommandHandler {
    val baseUrl = "/api/seqexec/commands"
    def handle(args: List[String], terminal: Terminal):js.Any
  }

  case class Command(handler: CommandHandler, description: String)

  def pause(t: Terminal): Future[Unit] = Future.apply(t.pause())
  def resume(t: Terminal): Future[Unit] = Future.apply(t.resume())
  def runInBackground[A](a: => Future[A], t: Terminal): Future[A] =
    for {
      _ <- pause(t)
      f <- a
      _ <- resume(t)
    } yield f
  
  object HostHandler extends CommandHandler {
    override def handle(args: List[String], terminal: Terminal): Any = {
      runInBackground(Ajax.get(s"$baseUrl/host"), terminal).onComplete {
        case Success(s) =>
          val r = read[RegularCommand](s.responseText)
          if (r.error) terminal.error(r.response) else terminal.echo(r.response)
        case Failure(s) => terminal.error(s.toString)
      }
    }
  }

  val commands: Map[String, Command] = Map(
    "host" -> Command(HostHandler, "Returns the odb host used by the seqexec")
  )

  val terminalHandler:(String, Terminal) => js.Any = { (command, terminal) =>
    val tokens = command.split(" ").toList
    tokens match {
      case cmd :: args if commands.contains(cmd)         => commands.get(command).foreach(_.handler.handle(args, terminal))
      case "help" :: Nil                                 => terminal.echo(s"Commands available: ${commands.keys.mkString(" ")}")
      case "help" :: cmd :: _  if commands.contains(cmd) => terminal.echo(s"help: ${~commands.get(cmd).map(_.description)}")
      case "" :: Nil                                     => // Ignore
      case cmd :: _                                      => terminal.error(s"Command '$command' unknown")
    }
  }

  override def main(): Unit = {
    $(document.body).terminal(terminalHandler, JsTerminalOptions
      .prompt("seqexec> ")
      .greetings("Seqexec terminal\n================")
      .completion(true))
  }
}
