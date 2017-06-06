package gem.ocs2

import gem.{Dataset, Observation, Program, Step}
import gem.config._
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
    fromParse { Parsers.datasetLabel }

  implicit val ObservationIdDecoder: PioDecoder[Observation.Id] =
    fromParse { Parsers.obsId }

  implicit val ProgramIdDecoder: PioDecoder[Program.Id] =
    fromParse { Parsers.progId }

  implicit val InstrumentDecoder: PioDecoder[Instrument] =
    fromParse { Parsers.instrument }

  implicit val StaticConfigDecoder: PioDecoder[StaticConfig] =
    fromParse { Parsers.instrument } .map {
      case Instrument.Phoenix    => PhoenixStaticConfig()
      case Instrument.Michelle   => MichelleStaticConfig()
      case Instrument.Gnirs      => GnirsStaticConfig()
      case Instrument.Niri       => NiriStaticConfig()
      case Instrument.Trecs      => TrecsStaticConfig()
      case Instrument.Nici       => NiciStaticConfig()
      case Instrument.Nifs       => NifsStaticConfig()
      case Instrument.Gpi        => GpiStaticConfig()
      case Instrument.Gsaoi      => GsaoiStaticConfig()
      case Instrument.GmosS      => GmosSStaticConfig()
      case Instrument.AcqCam     => AcqCamStaticConfig()
      case Instrument.GmosN      => GmosNStaticConfig()
      case Instrument.Bhros      => BhrosStaticConfig()
      case Instrument.Visitor    => VisitorStaticConfig()
      case Instrument.Flamingos2 => Flamingos2StaticConfig(false) // TODO
    }

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
  implicit val DatasetsDecoder: PioDecoder[List[Dataset]] =
    PioDecoder { n =>
      (n \\* "obsExecLog" \\* "&datasets" \\* "&dataset").decode[Dataset]
    }

  implicit val ObservationDecoder: PioDecoder[Option[Observation[StaticConfig, Step[DynamicConfig]]]] =
    PioDecoder { n =>
      for {
        id <- (n \! "@name"                ).decode[Observation.Id]
        t  <- (n \! "data" \? "#title"     ).decodeOrZero[String]
        i  <- (n \? "instrument" \! "@type").decode[StaticConfig]
        s  <- (n \! "sequence"             ).decode[List[Step[DynamicConfig]]](SequenceDecoder)
      } yield i.map(i => Observation(id, t, i, s))
    }

  implicit val ProgramDecoder: PioDecoder[Program[Observation[StaticConfig, Step[DynamicConfig]]]] =
    PioDecoder { n =>
      for {
        id <- (n \! "@name"           ).decode[Program.Id]
        t  <- (n \! "data" \? "#title").decodeOrZero[String]
        os <- (n \\* "observation"    ).decode[Option[Observation[StaticConfig, Step[DynamicConfig]]]]
      } yield Program(id, t, os.flatten)
    }
}
