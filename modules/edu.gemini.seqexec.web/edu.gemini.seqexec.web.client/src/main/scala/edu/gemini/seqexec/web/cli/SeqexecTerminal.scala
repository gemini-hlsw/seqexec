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
import upickle.default._

@JSExport("SeqexecTerminal")
object SeqexecTerminal extends js.JSApp {
  type CommandFunction = (List[String], Terminal) => js.Any

  trait CommandHandler {
    val baseUrl = "/api/seqexec/commands"
    def handle(args: List[String], terminal: Terminal):js.Any
  }

  case class Command(handler: CommandHandler, description: String)

  object HostHandler extends CommandHandler {
    override def handle(args: List[String], terminal: Terminal): Any = {
      Ajax.get(s"$baseUrl/host").onComplete {
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
    commands.get(command).map { h =>
      h.handler.handle(Nil, terminal)
    }.getOrElse {
      // if it is help show a list of the commands
      if (command == "help") {
        terminal.echo(s"Commands available: ${commands.keys.mkString(" ")}")
      } else {
        terminal.error(s"Command '$command' unknown")
      }
    }
  }

  override def main(): Unit = {
    $(document.body).terminal(terminalHandler, JsTerminalOptions
      .prompt("seqexec> ")
      .greeting(false)
      .completion(true))
  }
}
