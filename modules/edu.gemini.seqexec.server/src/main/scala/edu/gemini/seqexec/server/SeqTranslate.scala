package edu.gemini.seqexec.server

import java.time.LocalDate

import edu.gemini.seqexec.engine.{Action, Result, Sequence, Step}
import edu.gemini.seqexec.model.Model.SequenceMetadata
import edu.gemini.seqexec.server.SeqexecFailure.UnrecognizedInstrument
import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.spModel.config2.{Config, ConfigSequence, ItemKey}
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.seqcomp.SeqConfigNames._

import scalaz.Scalaz._
import scalaz._

/**
  * Created by jluhrs on 9/14/16.
  */
object SeqTranslate {

  case class Systems(
                    dhs: DhsClient,
                    tcs: TcsController,
                    flamingos2: Flamingos2Controller
                    )

  implicit def toAction(x: SeqAction[Result.Response]): Action = x.run map {
    case -\/(e) => Result.Error(SeqexecFailure.explain(e))
    case \/-(r) => Result.OK(r)
  }

  private def step(systems: Systems)(i: Int, config: Config): SeqexecFailure \/ Step[Action] = {

    def buildStep(inst: Instrument): Step[Action] = {
      val sys = List(Tcs(systems.tcs), inst)
      val headers = List(new StandardHeader(systems.dhs, DummyObsKeywordsReader, DummyTcsKeywordsReader))

      Step[Action](
        i,
        None,
        config.toStepConfig,
        false,
        List(
          sys.map(x => toAction(x.configure(config).map(y => Result.Configured(y.sys.name)))),
          List(toAction(inst.observe(config)((systems.dhs, headers)).map(x => Result.Observed(x.dataId))))
        )
      )
    }

    val instName = extractInstrumentName(config)

    instName match {
      case Flamingos2.name => buildStep(Flamingos2(systems.flamingos2)).right
      case _               => UnrecognizedInstrument(instName.toString).left[Step[Action]]
    }

  }

  private def extractInstrumentName(config: Config): String =
    // This is too weak. We may want to use the extractors used in ITC
    config.getItemValue(new ItemKey(INSTRUMENT_KEY, INSTRUMENT_NAME_PROP)).toString

  def sequence(systems: Systems)(obsId: String, sequenceConfig: ConfigSequence): SeqexecFailure \/ Sequence[Action] = {
    val configs = sequenceConfig.getAllSteps.toList

    val steps = configs.zipWithIndex.traverseU {
      case (c, i) => step(systems)(i, c)
    }

    val instName = configs.headOption.map(extractInstrumentName).getOrElse("Unknown instrument")

    steps.map(Sequence[Action](obsId, SequenceMetadata(instName), _))
  }

}

