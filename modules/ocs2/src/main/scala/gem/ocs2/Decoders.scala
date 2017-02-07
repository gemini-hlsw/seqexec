package gem.ocs2

import gem.{Dataset, Observation, Program, Step}
import gem.config.InstrumentConfig
import gem.enum.Instrument
import gem.ocs2.pio.PioPath._
import gem.ocs2.pio.PioDecoder
import gem.ocs2.pio.PioDecoder.fromParse

import java.time.Instant

import scalaz._
import Scalaz._

/** `PioDecoder` instances for our model types.
  */
object Decoders {
  implicit val DatasetLabelDecoder: PioDecoder[Dataset.Label] =
    fromParse("Dataset.Label") { Parsers.datasetLabel }

  implicit val ObservationIdDecoder: PioDecoder[Observation.Id] =
    fromParse("Observation.Id") { Parsers.obsId }

  implicit val ProgramIdDecoder: PioDecoder[Program.Id] =
    fromParse("ProgramId") { Parsers.progId }

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

  // Decodes all the datasets in either a program or observation node.  We have
  // to explicitly specify that we want the "dataset" elements within a
  // "datasets" paramset because they are duplicated in the event data.
  val DatasetsDecoder: PioDecoder[List[Dataset]] =
    PioDecoder { n =>
      (n \\* "obsExecLog" \\* "&datasets" \\* "&dataset").decode[Dataset]
    }

  implicit val ObservationDecoder: PioDecoder[Observation[Step[InstrumentConfig]]] =
    PioDecoder { n =>
      for {
        id <- (n \! "@name"                ).decode[Observation.Id]
        t  <- (n \! "data" \? "#title"     ).decodeOrZero[String]
        i  <- (n \? "instrument" \! "@type").decode[Instrument]
        s  <- (n \! "sequence"             ).decode[List[Step[InstrumentConfig]]](SequenceDecoder)
      } yield Observation(id, t, i, s)
    }

  implicit val ProgramDecoder: PioDecoder[Program[Observation[Step[InstrumentConfig]]]] =
    PioDecoder { n =>
      for {
        id <- (n \! "@name"           ).decode[Program.Id]
        t  <- (n \! "data" \? "#title").decodeOrZero[String]
        os <- (n \\* "observation"    ).decode[Observation[Step[InstrumentConfig]]]
      } yield Program(id, t, os)
    }
}
