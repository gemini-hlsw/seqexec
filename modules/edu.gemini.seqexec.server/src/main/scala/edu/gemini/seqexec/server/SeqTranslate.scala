package edu.gemini.seqexec.server

import edu.gemini.seqexec.engine.{Action, Result, Sequence, Step}
import edu.gemini.seqexec.server.SeqexecFailure.{Execution, UnrecognizedInstrument}
import edu.gemini.spModel.config2.{Config, ConfigSequence, ItemKey}
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.seqcomp.SeqConfigNames._

import scalaz.\/
import scalaz.NonEmptyList._
import scalaz._
import Scalaz._
import scalaz.syntax.TraverseOps

/**
  * Created by jluhrs on 9/14/16.
  */
object SeqTranslate {

  implicit def toAction[A](x: SeqAction[A]): Action = x.run map {
    case -\/(e) => Result.Error(e)
    case \/-(r) => Result.OK(r)
  }

  def step(dhsClient: DhsClient)(i: Int, config: Config): SeqexecFailure \/ Step[Action] = {
    val instName = config.getItemValue(new ItemKey(INSTRUMENT_KEY, INSTRUMENT_NAME_PROP))
    val instrument = instName match {
      case GmosSouth.name => Some(GmosSouth)
      case Flamingos2.name => Some(Flamingos2(Flamingos2ControllerSim))
      case _ => None
    }

    instrument.map { a =>
      val systems = nels(Tcs(TcsControllerEpics), a)
      // TODO Find a proper way to inject the subsystems
      Step[Action](i, nels(systems.map(x => toAction(x.configure(config))), nels(toAction(a.observe(config)(dhsClient))))).right
    }.getOrElse(UnrecognizedInstrument(instName.toString).left[Step[Action]])
  }

  def sequence(dhsClient: DhsClient)(obsId: String, sequenceConfig: ConfigSequence): SeqexecFailure \/ Sequence[Action] =
  {
    def nelOfConfigs(l : List[Config]): SeqexecFailure \/ NonEmptyList[Config] =
      l.toNel.map(_.right).getOrElse(Execution("No steps found in sequence.").left)

    def steps(l : NonEmptyList[Config]): SeqexecFailure \/ NonEmptyList[Step[Action]] =
      l.zipWithIndex.traverse[({ type λ[β] = SeqexecFailure \/ β })#λ, Step[Action]] { case (c, i) => step(dhsClient)(i, c) }

    val a = sequenceConfig.getAllSteps.toList

    val b = nelOfConfigs(a).flatMap(steps)

    b.map(Sequence[Action](obsId, _, List()))
  }
}
