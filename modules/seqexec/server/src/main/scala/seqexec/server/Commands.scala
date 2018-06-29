// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.implicits._
import edu.gemini.spModel.`type`.{DisplayableSpType, LoggableSpType, SequenceableSpType}
import edu.gemini.spModel.config2.{ConfigSequence, ItemKey}
import edu.gemini.spModel.core.Peer
import gem.Observation

sealed trait CommandError {
  val msg: String
}
final case class BadParameter(msg: String) extends CommandError
final case class ObsIdNotFound(msg: String) extends CommandError
final case class SeqexecFailureError(e: SeqexecFailure) extends CommandError {
  val msg: String = SeqexecFailure.explain(e)
}

final case class CommandResponse(msg: String, keys: List[(String, String)], steps: List[String])

object CommandResponse {
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(msg: String): CommandResponse = CommandResponse(msg, Nil, Nil)
}

@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
sealed trait Commands {
  import Commands.CommandResult

  def host(): CommandResult
  def showCount(obsId: String): CommandResult
  def showStatic(obsId: String): CommandResult
  def showStatic(obsId: String, system: String): CommandResult
  def showDynamic(obsId: String, step: String): CommandResult
  def showDynamic(obsId: String, step: String, system: String): CommandResult

}

object Commands {
  type CommandResult = Either[CommandError, CommandResponse]

  val Usage: String =
    "Usage: seq host [host:port] | show obsId count|static|dynamic | run obsId"
  val ShowUsage: String =
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

  // scalastyle:off
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(odbProxy: ODBProxy): Commands = new Commands {

    override def host(): CommandResult =
      CommandResponse(s"Default seq host set to ${odbProxy.host().host} ${odbProxy.host().port}").asRight

    override def showCount(obsId: String): CommandResult =
      for {
        oid <- parseId(obsId)
        seq <- odbProxy.read(oid).leftMap(SeqexecFailureError.apply)
      } yield CommandResponse(s"$oid sequence has ${seq.config.size()} steps.")

    override def showStatic(obsId: String): CommandResult =
      for {
        oid <- parseId(obsId)
        seq <- odbProxy.read(oid).leftMap(SeqexecFailureError.apply)
      } yield CommandResponse(s"$oid Static Values", keys(seq.config, 0, seq.config.getStaticKeys), Nil)

    override def showStatic(obsId: String, system: String): CommandResult =
      for {
        oid <- parseId(obsId)
        seq <- odbProxy.read(oid).leftMap(SeqexecFailureError.apply)
        ks  = seq.config.getStaticKeys.filter(sysFilter(system))
      } yield CommandResponse(s"$oid Static Values ($system only)", keys(seq.config, 0, ks), Nil)

    override def showDynamic(obsId: String, step: String): CommandResult =
      for {
        oid <- parseId(obsId)
        seq <- odbProxy.read(oid).leftMap(SeqexecFailureError.apply)
        s   <- ifStepValid(oid, seq.config, step)
      } yield CommandResponse(s"$oid Dynamic Values (Step ${s + 1})", keys(seq.config, s, seq.config.getIteratedKeys), Nil)

    override def showDynamic(obsId: String, step: String, system: String): CommandResult =
      for {
        oid <- parseId(obsId)
        seq <- odbProxy.read(oid).leftMap(SeqexecFailureError.apply)
        s   <- ifStepValid(oid, seq.config, step)
        ks  = seq.config.getStaticKeys.filter(sysFilter(system))
      } yield CommandResponse(s"$oid Dynamic Values (Step ${s + 1}, $system only)", keys(seq.config, s, ks), Nil)

    def seqValue(o: Object): String = o match {
      case s: SequenceableSpType => s.sequenceValue()
      case d: DisplayableSpType  => d.displayValue()
      case l: LoggableSpType     => l.logValue()
      case _                     => s"$o"
    }

    def keys(cs: ConfigSequence, step: Int, ks: Array[ItemKey]): List[(String, String)] = {
      ks.sortWith((u, v) => u.compareTo(v) < 0).map { k =>
        k.getPath -> seqValue(cs.getItemValue(step, k))
      }.toList
    }

    def sysFilter(system: String): ItemKey => Boolean =
      _.splitPath().get(0) === system

    def ifStepValid(oid: Observation.Id, cs: ConfigSequence, step: String): Either[CommandError, Int] =
      Either.catchNonFatal(step.toInt - 1).fold(
      _ => BadParameter(s"Specify an integer step, not '$step'.").asLeft, {
        case i if i < 0          => BadParameter("Specify a positive step number.").asLeft
        case i if i >= cs.size() => BadParameter(s"${oid.format} only has ${cs.size} steps.").asLeft
        case i                   => Right(i)
      })
  }
  // scalastyle:on

  def parseId(s: String): Either[CommandError, Observation.Id] =
    Observation.Id.fromString(s).toRight(BadParameter(s"Sorry, '$s' isn't a valid observation id."))

  def parseLoc(s: String): Either[CommandError, Peer] =
    Option(Peer.tryParse(s)).toRight(BadParameter(s"Sorry, expecting host:port not '$s'."))

}
