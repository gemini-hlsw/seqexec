package gem.seqimporter

import edu.gemini.spModel.core.ProgramId
import gem.{Dataset, Observation, Program, Step}
import gem.config.InstrumentConfig
import gem.enum.Instrument
import gem.seqimporter.pio.PioPath._
import gem.seqimporter.pio.PioDecoder
import gem.seqimporter.pio.PioDecoder.fromParse

import java.time.Instant

/** `PioDecoder` instances for our model types.
  */
object Decoders {
  implicit val DatasetLabelDecoder: PioDecoder[Dataset.Label] =
    fromParse("Dataset.Label") { Dataset.Label.fromString }

  implicit val ObservationIdDecoder: PioDecoder[Observation.Id] =
    fromParse("Observation.Id") { Parsers.obsId }

  implicit val ProgramIdDecoder: PioDecoder[Program.Id] =
    fromParse("ProgramId") { s => Option(ProgramId.parse(s)) }

  implicit val InstrumentDecoder: PioDecoder[Instrument] =
    fromParse("Instrument") { Parsers.instrument }

  implicit val DatasetDecoder: PioDecoder[Dataset] =
    PioDecoder { n =>
      for {
        l <- (n \! "#datasetLabel").decode[Dataset.Label]
        f <- (n \! "#dhsFilename" ).decode[String]
        t <- (n \! "#timestamp"   ).decode[Instant]
      } yield Dataset(l, f, t)
    }

  implicit val ObservationDecoder: PioDecoder[Observation[Step[InstrumentConfig]]] =
    PioDecoder { n =>
      for {
        id <- (n \! "@name"                ).decode[Observation.Id]
        t  <- (n \! "data" \! "#title"     ).decode[String]
        i  <- (n \? "instrument" \! "@type").decode[Instrument]
        s  <- (n \! "sequence"             ).decode[List[Step[InstrumentConfig]]](SequenceDecoder)
      } yield Observation(id, t, i, s)
    }

  implicit val ProgramDecoder: PioDecoder[Program[Observation[Step[InstrumentConfig]]]] =
    PioDecoder { n =>
      for {
        id <- (n \! "@name"           ).decode[Program.Id]
        t  <- (n \! "data" \! "#title").decode[String]
        os <- (n \\* "observation"    ).decode[Observation[Step[InstrumentConfig]]]
      } yield Program(id, t, os)
    }
}
