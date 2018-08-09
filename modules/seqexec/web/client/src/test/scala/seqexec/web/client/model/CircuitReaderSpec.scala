// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import gem.Observation
import seqexec.model.enum.Instrument
import seqexec.web.client.circuit.SeqexecCircuit._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class CircuitReaderSpec extends FlatSpec with Matchers with PropertyChecks with ArbitrariesWebClient {
  import seqexec.model.SharedModelArbitraries._

  "CircuitReaderSpec" should
    "maintain reference equality for constant readers" in {
      (webSocketFocusRW === webSocketFocusRW.value) should be(true)
      (statusAndLoadedSequencesReader === statusAndLoadedSequencesReader.value) should be(true)
      (statusReader === statusReader.value) should be(true)
      (headerSideBarReader === headerSideBarReader.value) should be(true)
      (logDisplayedReader === logDisplayedReader.value) should be(true)
    }
    it should "maintain reference equality for instrument based readers" in {
      forAll{ (i: Instrument) =>
        (instrumentTab(i) === instrumentTab(i).value) should be(true)
        (instrumentStatusReader(i) === instrumentStatusReader(i).value) should be(true)
        (instrumentTabContentReader(i) === instrumentTabContentReader(i).value) should be(true)
        (sequenceObserverReader(i) === sequenceObserverReader(i).value) should be(true)
        (statusAndStepReader(i) === statusAndStepReader(i).value) should be(true)
        (stepsTableReaderF(i) === stepsTableReaderF(i).value) should be(true)
        (stepsTableReader(i) === stepsTableReader(i).value) should be(true)
        (sequenceControlReader(i) === sequenceControlReader(i).value) should be(true)
      }
    }
    it should "maintain reference equality for the sequence reader" in {
      forAll{ (i: Observation.Id) =>
        (sequenceReader(i) === sequenceReader(i).value) should be(true)
      }
    }
}
