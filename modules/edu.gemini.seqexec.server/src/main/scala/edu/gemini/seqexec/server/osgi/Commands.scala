package edu.gemini.seqexec.server.osgi

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.server._
import edu.gemini.seqexec.shared.{SeqExecService, SeqFailure}
import edu.gemini.spModel.`type`.{DisplayableSpType, LoggableSpType, SequenceableSpType}
import edu.gemini.spModel.config2.{ConfigSequence, ItemKey}
import edu.gemini.spModel.core.Peer

import scalaz._
import Scalaz._


sealed trait Commands {
  def seq(cmd: String, args: Array[String]): String
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

    def host(): String =
      s"Default seq host set to ${ExecutorImpl.host.host} ${ExecutorImpl.host.port}"

    def host(loc0: Peer): String = {
      ExecutorImpl.host(loc0)
      host()
    }

    def show(oid: SPObservationID, cs: ConfigSequence, args: List[String]): String = {
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
          val paddedKey = s"%-${pad}s".format(k.toString)
          s"$paddedKey -> ${seqValue(cs.getItemValue(step, k))}"
        }.mkString(s"$title\n", "\n", "")
      }

      def sysFilter(system: String): ItemKey => Boolean =
        _.splitPath().get(0) == system

      def ifStepValid(step: String)(body: Int => String): String =
        \/.fromTryCatchNonFatal(step.toInt - 1).fold(
        _ => s"Specify an integer step, not '$step'.", {
          case i if i < 0 => "Specify a positive step number."
          case i if i >= cs.size() => s"$oid only has ${cs.size} steps."
          case i => body(i)
        })


      args match {
        case List("count") =>
          s"$oid sequence has ${cs.size()} steps."

        case List("static") =>
          showKeys(s"$oid Static Values", 0, cs.getStaticKeys)

        case List("static", system) =>
          val ks = cs.getStaticKeys.filter(sysFilter(system))
          showKeys(s"$oid Static Values ($system only)", 0, ks)

        case List("dynamic", step) =>
          ifStepValid(step) { s =>
            showKeys(s"$oid Dynamic Values (Step ${s + 1})", s, cs.getIteratedKeys)
          }

        case List("dynamic", step, system) =>
          ifStepValid(step) { s =>
            val ks = cs.getIteratedKeys.filter(sysFilter(system))
            showKeys(s"$oid Dynamic Values (Step ${s + 1}, $system only)", s, ks)
          }

        case _ =>
          ShowUsage
      }
    }

    def seq(cmd: String, args: Array[String]): String =
      cmd :: args.toList match {
        case List("host") =>
          host()

        case List("host", peer) =>
          parseLoc(peer).map(host).merge

        case "show" :: obsId :: showArgs =>
          (for {
            oid <- parseId(obsId)
            seq <- ExecutorImpl.read(oid).leftMap(SeqexecFailure.explain(_))
          } yield show(oid, seq, showArgs)).merge

        case List("run", obsId) =>
          (for {
            oid <- parseId(obsId)
            _ <- ExecutorImpl.start(oid).leftMap(SeqexecFailure.explain(_))
          } yield s"Sequence $obsId started." ) .merge

        case List("stop", obsId) =>
          (for {
            oid <- parseId(obsId)
            _ <- ExecutorImpl.stop(oid).leftMap(SeqexecFailure.explain(_))
          } yield s"Stop requested for $obsId." ) .merge

        case List("continue", obsId) => (
          for {
            oid <- parseId(obsId)
            _ <- ExecutorImpl.continue(oid).leftMap(SeqexecFailure.explain(_))
          } yield s"Resume requested for $obsId." ) .merge

        case List("state", obsId) => (
          for {
            oid <- parseId(obsId)
            s <- ExecutorImpl.state(oid).leftMap(SeqexecFailure.explain(_))
          } yield ExecutorImpl.stateDescription(s) ) .merge

        case _ =>
          Usage
      }
  }

  def parseId(s: String): String \/ SPObservationID =
    \/.fromTryCatchNonFatal {
      new SPObservationID(s)
    }.leftMap(_ => s"Sorry, '$s' isn't a valid observation id.")

  def parseLoc(s: String): String \/ Peer =
    Option(Peer.tryParse(s)) \/> s"Sorry, expecting host:port not '$s'."

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
          case Executor.Ok(StepResult(_, ObserveResult(id))) => id
          case _                                             => ""
        }.filter(!_.isEmpty).mkString("\n"))
        r match {
          case -\/(errs) => println(errs.toList.map(SeqexecFailure.explain).mkString(","))
          case _         => ()
        }
    }

  }
}
