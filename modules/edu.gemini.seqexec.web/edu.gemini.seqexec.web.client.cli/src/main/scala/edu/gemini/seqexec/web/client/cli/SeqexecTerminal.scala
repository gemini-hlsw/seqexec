package edu.gemini.seqexec.web.client.cli

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import js.JSConverters._
import org.querki.jquery.$
import org.scalajs.dom.document
import org.scalajs.dom.ext.Ajax
import JQueryTerminal.{Terminal, _}
import edu.gemini.seqexec.web.common.{CliCommand, SequenceConfig, StepConfig}
import org.scalajs.dom

import scala.util.{Failure, Success, Try}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import boopickle.Default._

import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}

@JSExport("SeqexecTerminal")
object SeqexecTerminal extends js.JSApp {

  trait CommandHandler {
    val baseUrl = "/api/seqexec/commands"
    def handle(args: List[String], terminal: Terminal):Unit

    def complete[A <: dom.XMLHttpRequest, B](terminal: Terminal, f: CliCommand => B): PartialFunction[Try[A], js.Any] = {
      case Success(s) =>
        val ab = TypedArrayBuffer.wrap(s.response.asInstanceOf[ArrayBuffer])
        val r  = Unpickle[CliCommand].fromBytes(ab)
        if (r.error) terminal.error(r.response) else terminal.echo(f(r).toString)
      case Failure(s) => terminal.error(s.toString)
    }

    def pause(t: Terminal) = Future.apply(t.pause())
    def resume(t: Terminal) = Future.apply(t.resume())

    def runInBackground[A <: dom.XMLHttpRequest, B](a: => Future[A], f: CliCommand => B, t: Terminal): Unit =
      (for {
        _ <- pause(t)
        f <- a.andThen(complete(t, f))
      } yield f).andThen {
        case u => resume(t) // resume regardless of whether there is an error
      }
  }

  def defaultResponse(c: CliCommand) = c.response

  case class Command(cmd: String, args: Int, handler: CommandHandler, description: List[String])

  object HostHandler extends CommandHandler {
    override def handle(args: List[String], terminal: Terminal): Unit = {
      runInBackground(Ajax.get(s"$baseUrl/host", responseType = "arraybuffer"), defaultResponse, terminal)
    }
  }

  object SetHostHandler extends CommandHandler {
    override def handle(args: List[String], terminal: Terminal): Unit = args match {
      case host :: Nil =>
        runInBackground(Ajax.post(s"$baseUrl/host",
          data = s"host=$host",
          responseType = "arraybuffer",
          headers = Map("Content-Type" -> "application/x-www-form-urlencoded")), defaultResponse, terminal)
      case _           =>
    }
  }

  object RunHandler extends CommandHandler {
    override def handle(args: List[String], terminal: Terminal): Unit = args match {
      case obsId :: Nil =>
        runInBackground(Ajax.post(s"$baseUrl/$obsId/run", responseType = "arraybuffer"), defaultResponse, terminal)
      case _            =>
    }
  }

  object StopHandler extends CommandHandler {
    override def handle(args: List[String], terminal: Terminal): Unit = args match {
      case obsId :: Nil =>
        runInBackground(Ajax.post(s"$baseUrl/$obsId/stop", responseType = "arraybuffer"), defaultResponse, terminal)
      case _            =>
    }
  }

  object ContinueHandler extends CommandHandler {
    override def handle(args: List[String], terminal: Terminal): Unit = args match {
      case obsId :: Nil =>
        runInBackground(Ajax.post(s"$baseUrl/$obsId/continue", responseType = "arraybuffer"), defaultResponse, terminal)
      case _            =>
    }
  }

  object RunStateHandler extends CommandHandler {
    override def handle(args: List[String], terminal: Terminal): Unit = args match {
      case obsId :: Nil =>
        runInBackground(Ajax.get(s"$baseUrl/$obsId/state", responseType = "arraybuffer"), defaultResponse, terminal)
      case _            =>
    }
  }

  object ShowHandler extends CommandHandler {
    def width(ks: List[StepConfig]): Int = ks match {
      case Nil => 0
      case _   => ks.map(_.key.length).max
    }

    def staticResponse(c: CliCommand) = c match {
      case SequenceConfig(_, _, _, k) =>
        val pad = width(k)
        k.sortBy(_.key).map(s => {
          val paddedKey = s"%-${pad}s".format(s.key)
          s"$paddedKey -> ${s.value}"}).mkString("\n")
      case _                          => defaultResponse(c)
    }

    override def handle(args: List[String], terminal: Terminal): Unit = args match {
      case obsId :: "count" :: Nil                       =>
        runInBackground(Ajax.get(s"$baseUrl/${args.head}/count", responseType = "arraybuffer"), defaultResponse, terminal)
      case obsId :: "static" :: Nil                      =>
        runInBackground(Ajax.get(s"$baseUrl/${args.head}/static", responseType = "arraybuffer"), staticResponse, terminal)
      case obsId :: "static" :: subsystem :: Nil         =>
        runInBackground(Ajax.get(s"$baseUrl/${args.head}/static/$subsystem", responseType = "arraybuffer"), staticResponse, terminal)
      case obsId :: "dynamic":: step :: Nil              =>
        runInBackground(Ajax.get(s"$baseUrl/${args.head}/dynamic/$step", responseType = "arraybuffer"), staticResponse, terminal)
      case obsId :: "dynamic":: step :: subsystem :: Nil =>
        runInBackground(Ajax.get(s"$baseUrl/${args.head}/dynamic/$step/$subsystem", responseType = "arraybuffer"), staticResponse, terminal)
      case _                                             =>
        terminal.error("Unknown show command")
    }
  }

  def bold(s: String):String = s"[[b;;]$s]"
  def italic(s: String):String = s"[[ig;;]$s]"

  // Maps the command text and amount of args required to command handler
  val commands: List[Command] = List(
    Command("host", 0, HostHandler, List(
      s"${bold("host")}: Returns the odb host used by the seqexec")),
    Command("host", 1, SetHostHandler, List(
      s"${bold("host")} ${bold("host:port")}: Sets the odb host:port used by the seqexec")),
    Command("run", 1, RunHandler, List(
      s"${bold("run")} ${italic("obsId")}: Runs obs id")),
    Command("stop", 1, StopHandler, List(
      s"${bold("stop")} ${italic("obsId")}: Request to stop running obs id")),
    Command("continue", 1, ContinueHandler, List(
      s"${bold("continue")} ${italic("obsId")}: Continue stopped obs id")),
    Command("state", 1, RunStateHandler, List(
      s"${bold("state")} ${italic("obsId")}: Request the currently running state of obs id")),
    Command("show", 2, ShowHandler, List(
      s"${bold("show")} ${italic("obsId count")}: Show obs id steps count",
      s"${bold("show")} ${italic("obsId static")}: Show static configuration for obs id",
      s"${bold("show")} ${italic("obsId dynamic #step")}: Show dynamic configuration for obs id and #step")),
    Command("show", 3, ShowHandler, List(
      s"${bold("show")} ${italic("obsId static subsystem")}: Show static configuration for subsystem")),
    Command("show", 4, ShowHandler, List(
      s"${bold("show")} ${italic("obsId dynamic #step subsystem")}: Show dynamic configuration for obs id, #step and subsystem"))
  )

  // Used for tab completion
  val cmdStrings: Seq[String] = "help" :: commands.map(_.cmd).distinct

  val terminalHandler:(String, Terminal) => js.Any = { (command, terminal) =>
    val tokens = command.split(" ").toList

    def find(cmd: String, args: List[String]): Option[Command] = commands.find(c => c.cmd == cmd && c.args == args.size)
    def findSimilar(cmd: String): Option[Command] = commands.find(c => c.cmd == cmd)

    tokens match {
      case cmd :: args if find(cmd, args).isDefined             =>
        find(cmd, args).foreach(_.handler.handle(args, terminal))
      case cmd :: args if findSimilar(cmd).isDefined            =>
        findSimilar(cmd).foreach(c => terminal.echo(s"Incomplete command ${bold(cmd)}: Usage:\n${c.description.mkString("\n")}"))
      case "help" :: Nil                                        =>
        terminal.echo(s"Commands available: ${italic("exit clear")} ${commands.map(c => s"[[ig;;]${c.cmd}]").distinct.mkString(" ")}")
      case "help" :: cmd :: _  if commands.exists(_.cmd == cmd) =>
        terminal.echo(s"help:\n${commands.filter(_.cmd == cmd).flatMap(_.description).mkString("\n")}")
      case "" :: Nil                                            =>
        // Ignore
      case cmd :: _                                             =>
        terminal.error(s"Command '$command' unknown")
    }
  }

  override def main(): Unit = {
    $(document.body).terminal(terminalHandler, JsTerminalOptions
      .prompt("seqexec> ")
      .greetings(banner + s"\nVersion: ${OcsBuildInfo.version}\n")
      .completion((t: Terminal, c: String, callback: CompletionCallback) => callback(cmdStrings.toJSArray)))
  }

  val banner = """  ___
                 | / __| ___ __ _ _____ _____ __
                 | \__ \/ -_) _` / -_) \ / -_) _|
                 | |___/\___\__, \___/_\_\___\__|
                 |             |_|               """.stripMargin
}
