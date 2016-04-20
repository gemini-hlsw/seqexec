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
import upickle.default._

import scalaz._
import Scalaz._

@JSExport("SeqexecTerminal")
object SeqexecTerminal extends js.JSApp {

  trait CommandHandler {
    val baseUrl = "/api/seqexec/commands"
    def handle(args: List[String], terminal: Terminal):Unit

    def complete[A <: dom.XMLHttpRequest, B](terminal: Terminal, f: CliCommand => B): PartialFunction[Try[A], js.Any] = {
      case Success(s) =>
        val r = read[CliCommand](s.responseText)
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
      runInBackground(Ajax.get(s"$baseUrl/host"), defaultResponse, terminal)
    }
  }

  object SetHostHandler extends CommandHandler {
    override def handle(args: List[String], terminal: Terminal): Unit = {
      runInBackground(Ajax.post(s"$baseUrl/host",
        data = s"host=${args.head}",
        headers = Map("Content-Type" -> "application/x-www-form-urlencoded")), defaultResponse, terminal)
    }
  }

  object RunHandler extends CommandHandler {
    override def handle(args: List[String], terminal: Terminal): Unit = {
      runInBackground(Ajax.post(s"$baseUrl/${args.head}/run"), defaultResponse, terminal)
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

    override def handle(args: List[String], terminal: Terminal): Unit = {
      args match {
        case obsId :: "count" :: Nil  => runInBackground(Ajax.get(s"$baseUrl/${args.head}/count"), defaultResponse, terminal)
        case obsId :: "static" :: Nil => runInBackground(Ajax.get(s"$baseUrl/${args.head}/static"), staticResponse, terminal)
        case _                        => terminal.error("Unknown show command")
      }
    }
  }

  // Maps the command text and amount of args required to command handler
  val commands: List[Command] = List(
    Command("host", 0, HostHandler, List("[[b;;]host]: Returns the odb host used by the seqexec")),
    Command("host", 1, SetHostHandler, List("[[b;;]host] [[b;;]host:port]: Sets the odb host:port used by the seqexec")),
    Command("run", 1, RunHandler, List("[[b;;]run] [[ig;;]obsId]: Runs obs id")),
    Command("show", 2, ShowHandler, List("[[b;;]show] [[ig;;]obsId count]: Show obs id steps count", "[[b;;]show] [[ig;;]obsId static]: Runs obs id")),
    Command("show", 3, ShowHandler, List("[[b;;]show] [[ig;;]obsId static|dynamic] subsystem: Runs obs id"))
  )

  // Used for tab completion
  val cmdStrings: Seq[String] = "help" :: commands.map(_.cmd).distinct

  val terminalHandler:(String, Terminal) => js.Any = { (command, terminal) =>
    val tokens = command.split(" ").toList

    def find(cmd: String, args: List[String]): Option[Command] = commands.find(c => c.cmd === cmd && c.args == args.size)
    def findSimilar(cmd: String): Option[Command] = commands.find(c => c.cmd === cmd)

    tokens match {
      case cmd :: args if find(cmd, args).isDefined              => find(cmd, args).foreach(_.handler.handle(args, terminal))
      case cmd :: args if findSimilar(cmd).isDefined             => findSimilar(cmd).foreach(c => terminal.echo(s"Incomplete command: Usage ${c.description}"))
      case "help" :: Nil                                         => terminal.echo(s"Commands available: [[ig;;]exit] [[ig;;]clear] ${commands.map(c => s"[[ig;;]${c.cmd}]").distinct.mkString(" ")}")
      case "help" :: cmd :: _  if commands.exists(_.cmd === cmd) => terminal.echo(s"help:\n${commands.filter(_.cmd === cmd).flatMap(_.description).mkString("\n")}")
      case "" :: Nil                                             => // Ignore
      case cmd :: _                                              => terminal.error(s"Command '$command' unknown")
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
