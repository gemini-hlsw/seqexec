/*
 * Copyright (c) 2017, Association of Universities for Research in Astronomy, Inc. (AURA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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

  implicit val ObservationDecoder: PioDecoder[Observation[StaticConfig, Step[DynamicConfig]]] =
    PioDecoder { n =>
      for {
        id <- (n \! "@name"                ).decode[Observation.Id]
        t  <- (n \! "data" \? "#title"     ).decodeOrZero[String]
        st <- (n \! "sequence"             ).decode[StaticConfig](StaticDecoder)
        sq <- (n \! "sequence"             ).decode[List[Step[DynamicConfig]]](SequenceDecoder)
      } yield Observation(id, t, st, sq)
    }

  implicit val ProgramDecoder: PioDecoder[Program[Observation[StaticConfig, Step[DynamicConfig]]]] =
    PioDecoder { n =>
      for {
        id <- (n \! "@name"           ).decode[Program.Id]
        t  <- (n \! "data" \? "#title").decodeOrZero[String]
        os <- (n \\* "observation"    ).decode[Observation[StaticConfig, Step[DynamicConfig]]]
      } yield Program(id, t, os)
    }
}
