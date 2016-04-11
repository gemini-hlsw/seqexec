package edu.gemini.seqexec.web.cli

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import js.JSConverters._
import org.querki.jquery.$
import org.scalajs.dom.document
import org.scalajs.dom.ext.Ajax
import JQueryTerminal.{Terminal, _}
import edu.gemini.seqexec.web.common.RegularCommand
import edu.gemini.seqexec.web.client.{BuildInfo => buildinfo}

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

  case class Command(cmd: String, args: Int, handler: CommandHandler, description: String)

  def pause(t: Terminal): Future[Unit] = Future.apply(t.pause())
  def resume(t: Terminal): Future[Unit] = Future.apply(t.resume())
  def runInBackground[A](a: => Future[A], t: Terminal): Future[A] =
    (for {
      _ <- pause(t)
      f <- a
    } yield f).andThen {
      case _ => resume(t)
    }

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

  object SetHostHandler extends CommandHandler {
    override def handle(args: List[String], terminal: Terminal): Any = {
      runInBackground(Ajax.post(s"$baseUrl/host",
        data = s"host=${args.head}",
        headers = Map("Content-Type" -> "application/x-www-form-urlencoded")), terminal).onComplete {
        case Success(s) =>
          val r = read[RegularCommand](s.responseText)
          if (r.error) terminal.error(r.response) else terminal.echo(r.response)
        case Failure(s) => terminal.error(s.toString)
      }
    }
  }

  // Maps the command text and amount of args required to command handler
  val commands: List[Command] = List(
    Command("host", 0, HostHandler, "[[b;;]host]: Returns the odb host used by the seqexec"),
    Command("host", 1, SetHostHandler, "[[b;;]host] [[b;;]host:port]: Sets the odb host:port used by the seqexec")
  )

  // Used for tab completion
  val cmdStrings: Seq[String] = "help" :: commands.map(_.cmd).distinct

  val terminalHandler:(String, Terminal) => js.Any = { (command, terminal) =>
    val tokens = command.split(" ").toList

    def find(cmd: String, args: List[String]): Option[Command] = commands.find(c => c.cmd === cmd && c.args == args.size)

    tokens match {
      case cmd :: args if find(cmd, args).isDefined              => find(cmd, args).foreach(_.handler.handle(args, terminal))
      case "help" :: Nil                                         => terminal.echo(s"Commands available: ${commands.map(_.cmd).distinct.mkString(" ")}")
      case "help" :: cmd :: _  if commands.exists(_.cmd === cmd) => terminal.echo(s"help:\n${commands.filter(_.cmd === cmd).map(_.description).mkString("\n")}")
      case "" :: Nil                                             => // Ignore
      case cmd :: _                                              => terminal.error(s"Command '$command' unknown")
    }
  }

  override def main(): Unit = {
    $(document.body).terminal(terminalHandler, JsTerminalOptions
      .prompt("seqexec> ")
      .greetings(banner + s"\nVersion: ${buildinfo.version}\n")
      .completion((t: Terminal, c: String, n: CompletionCallback) => n(cmdStrings.toJSArray)))
  }

  val banner = """  ___
                 | / __| ___ __ _ _____ _____ __
                 | \__ \/ -_) _` / -_) \ / -_) _|
                 | |___/\___\__, \___/_\_\___\__|
                 |             |_|               """.stripMargin
}
