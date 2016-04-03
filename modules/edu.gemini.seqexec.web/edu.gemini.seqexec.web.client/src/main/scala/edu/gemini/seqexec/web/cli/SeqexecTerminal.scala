package edu.gemini.seqexec.web.cli

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import org.querki.jquery.$
import org.scalajs.dom.document
import JQueryTerminal.{Terminal, _}


@JSExport("SeqexecTerminal")
object SeqexecTerminal extends js.JSApp {
  type CommandHandler = (List[String], Terminal) => js.Any

  case class Command(cmd: Command, handler: CommandHandler, description: String)

  val commands: Map[String, CommandHandler] = Map.empty

  val terminalHandler:(String, Terminal) => js.Any = { (command, t) =>
    t.echo("command " + command)
  }

  override def main(): Unit = {
    $(document.body).terminal(terminalHandler, JsTerminalOptions
      .prompt("seqexec> ")
      .greeting(false)
      .completion(true))
  }
}
