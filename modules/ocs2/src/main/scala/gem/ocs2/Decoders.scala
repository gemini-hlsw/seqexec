// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2

import cats.implicits._
import gem.{Dataset, Observation, Program, Step}
import gem.config._
import gem.enum.Instrument
import gem.ocs2.pio.PioPath._
import gem.ocs2.pio.PioDecoder
import gem.ocs2.pio.PioDecoder.fromParse

import java.time.Instant

import scala.collection.immutable.TreeMap


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

  implicit val ObservationIndexDecoder: PioDecoder[Observation.Index] =
    PioDecoder { n =>
      (n \! "@name").decode[Observation.Id].map(_.index)
    }

  implicit val ObservationDecoder: PioDecoder[Observation[StaticConfig, Step[DynamicConfig]]] =
    PioDecoder { n =>
      for {
        id <- (n \! "@name"           ).decode[Observation.Id]
        t  <- (n \! "data" \? "#title").decodeOrZero[String]
        st <- (n \! "sequence"        ).decode[StaticConfig](StaticDecoder)
        sq <- (n \! "sequence"        ).decode[List[Step[DynamicConfig]]](SequenceDecoder)
      } yield Observation(id, t, st, sq)
    }

  implicit val ProgramDecoder: PioDecoder[Program[Observation[StaticConfig, Step[DynamicConfig]]]] =
    PioDecoder { n =>
      for {
        id <- (n \! "@name"           ).decode[Program.Id]
        t  <- (n \! "data" \? "#title").decodeOrZero[String]
        is <- (n \\* "observation"    ).decode[Observation.Index]
        os <- (n \\* "observation"    ).decode[Observation[StaticConfig, Step[DynamicConfig]]]
      } yield Program(id, t, TreeMap(is.zip(os): _*))
    }
}
