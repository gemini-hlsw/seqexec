package edu.gemini.seqexec.server.osgi

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.server._
import edu.gemini.spModel.`type`.{DisplayableSpType, LoggableSpType, SequenceableSpType}
import edu.gemini.spModel.config2.{ConfigSequence, ItemKey}
import edu.gemini.spModel.core.Peer

import scalaz._
import Scalaz._

sealed trait CommandError {
  val msg: String
}
case class BadParameter(msg: String) extends CommandError
case class ObsIdNotFound(msg: String) extends CommandError
case class SeqexecFailureError(e: SeqexecFailure) extends CommandError {
  val msg = SeqexecFailure.explain(e)
}

case class CommandResponse(msg: String, keys: List[(String, String)])

object CommandResponse {
  def apply(msg: String): CommandResponse = CommandResponse(msg, Nil)
}

sealed trait Commands {
  def seq(cmd: String, args: List[String]): CommandError \/ CommandResponse
}

object Commands {

  val Usage =
    "Usage: seq host [host:port] | show obsId count|static|dynamic | run obsId"
  val ShowUsage =
    """Usage:
      |  seq show obsId count
      |  seq show obsId static [calibration|instrument|telescope ...]
      |  seq show obsId dynamic step# [calibration|instrument|telescope ...]
      |  seq run obsId
      |  seq stop obsId
      |  seq continue obsId
      |  seq status obsId
      |      |
      |  Example
      |    seq show GS-2014B-Q-2-355 static
      |    -> shows all static values for observation 355
      |
      |    seq show GS-2014B-Q-2-355 static instrument
      |    -> shows all static instrument values for observation 355
      |
      |    seq show GS-2014B-Q-2-355 dynamic 4 instrument
      |    -> shows dynamic instrument values for dataset 4 of obs 355
    """.stripMargin

  def apply(): Commands = new Commands {

    def host(): CommandError \/ CommandResponse =
      \/.right(CommandResponse(s"Default seq host set to ${ExecutorImpl.host().host} ${ExecutorImpl.host().port}"))

    def host(loc0: Peer): CommandError \/ CommandResponse = {
      ExecutorImpl.host(loc0)
      host()
    }

    def show(oid: SPObservationID, cs: ConfigSequence, args: List[String]): CommandError \/ CommandResponse = {
      def seqValue(o: Object): String = o match {
        case s: SequenceableSpType => s.sequenceValue()
        case d: DisplayableSpType => d.displayValue()
        case l: LoggableSpType => l.logValue()
        case _ => o.toString
      }

      def width(ks: Array[ItemKey]): Int = ks match {
        case Array() => 0
        case _ => ks.maxBy(_.toString.length).toString.length
      }

      def showKeys(title: String, step: Int, ks: Array[ItemKey]): String = {
        val pad = width(ks)
        ks.sortWith((u, v) => u.compareTo(v) < 0).map { k =>
          val paddedKey = s"%-${pad}s".format(k)
          s"$paddedKey -> ${seqValue(cs.getItemValue(step, k))}"
        }.mkString(s"$title\n", "\n", "")
      }

      def keys(step: Int, ks: Array[ItemKey]): List[(String, String)] = {
        ks.sortWith((u, v) => u.compareTo(v) < 0).map { k =>
          k.getName -> seqValue(cs.getItemValue(step, k))
        }.toList
      }

      def sysFilter(system: String): ItemKey => Boolean =
        _.splitPath().get(0) == system

      def ifStepValid(step: String)(body: Int => CommandError \/ CommandResponse): CommandError \/ CommandResponse =
        \/.fromTryCatchNonFatal(step.toInt - 1).fold(
        _ => \/.left(BadParameter(s"Specify an integer step, not '$step'.")), {
          case i if i < 0          => \/.left(BadParameter("Specify a positive step number."))
          case i if i >= cs.size() => \/.left(BadParameter(s"$oid only has ${cs.size} steps."))
          case i                   => body(i)
        })

      args match {
        case List("count") =>
          \/.right(CommandResponse(s"$oid sequence has ${cs.size()} steps."))

        case List("static") =>
          \/.right(CommandResponse(s"$oid Static Values", keys(0, cs.getStaticKeys)))

        case List("static", system) =>
          val ks = cs.getStaticKeys.filter(sysFilter(system))
          \/.right(CommandResponse(s"$oid Static Values ($system only)", keys(0, ks)))

        case List("dynamic", step) =>
          ifStepValid(step) { s =>
            \/.right(CommandResponse(s"$oid Dynamic Values (Step ${s + 1})", keys(s, cs.getIteratedKeys)))
          }

        case List("dynamic", step, system) =>
          ifStepValid(step) { s =>
            val ks = cs.getIteratedKeys.filter(sysFilter(system))
            \/.right(CommandResponse(showKeys(s"$oid Dynamic Values (Step ${s + 1}, $system only)", s, ks)))
          }

        case _ =>
          \/.right(CommandResponse(ShowUsage))
      }
    }

    override def seq(cmd: String, args: List[String]): CommandError \/ CommandResponse =
      cmd :: args match {
        case List("host") =>
          host()

        case List("host", peer) =>
          parseLoc(peer).flatMap(host)

        case "show" :: obsId :: showArgs =>
          for {
            oid <- parseId(obsId)
            seq <- ExecutorImpl.read(oid).leftMap(SeqexecFailureError.apply)
            r   <- show(oid, seq, showArgs)
          } yield r

        case List("run", obsId) =>
          for {
            oid <- parseId(obsId)
            _   <- ExecutorImpl.start(oid).leftMap(SeqexecFailureError.apply)
            r   <- \/.right(CommandResponse(s"Sequence $obsId started."))
          } yield r

        case List("stop", obsId) =>
          for {
            oid <- parseId(obsId)
            _   <- ExecutorImpl.stop(oid).leftMap(SeqexecFailureError.apply)
            r   <- \/.right(CommandResponse(s"Stop requested for $obsId."))
          } yield r

        case List("continue", obsId) =>
          for {
            oid <- parseId(obsId)
            _   <- ExecutorImpl.continue(oid).leftMap(SeqexecFailureError.apply)
            r   <- \/.right(CommandResponse(s"Resume requested for $obsId."))
          } yield r

        case List("state", obsId) =>
          for {
            oid <- parseId(obsId)
            s   <- ExecutorImpl.state(oid).leftMap(SeqexecFailureError.apply)
            r   <- \/.right(CommandResponse(ExecutorImpl.stateDescription(s)))
          } yield r

        case _ =>
          \/.right(CommandResponse(Usage))
      }
  }

  def parseId(s: String): CommandError \/ SPObservationID =
    \/.fromTryCatchNonFatal {
      new SPObservationID(s)
    }.leftMap(_ => BadParameter(s"Sorry, '$s' isn't a valid observation id."))

  def parseLoc(s: String): CommandError \/ Peer =
    Option(Peer.tryParse(s)) \/> BadParameter(s"Sorry, expecting host:port not '$s'.")

  // a better version of this available in the latest scalaz
  implicit class MergeOp[A](d: A \/ A) {
    def merge: A = d.fold(identity, identity)
  }

  implicit object KeyOrdering extends scala.Ordering[ItemKey] {
    override def compare(x: ItemKey, y: ItemKey) = x.compareTo(y)
  }

  // TODO: this, better
  def onComplete[A](id: SPObservationID)(result: Throwable \/ A): Unit =
    println(id + ": " + result)

  // TODO: this, much, much better
  def onCompleteRun (id: SPObservationID)(result: Throwable \/ (Executor.ExecState, \/[NonEmptyList[SeqexecFailure], Unit])): Unit = {
    println(id + ": ")
    result match {
      case -\/(ex) => println(ex.getMessage)
      case \/-((st, r)) =>
        println(st.completed.map {
          case Executor.Ok(StepResult(_, ObserveResult(i))) => i
          case _                                            => ""
        }.filter(!_.isEmpty).mkString("\n"))
        r match {
          case -\/(errs) => println(errs.toList.map(SeqexecFailure.explain).mkString(","))
          case _         => ()
        }
    }

  }
}
