// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client

import edu.gemini.seqexec.model.Model.{SequenceId, Instrument}
import edu.gemini.seqexec.web.client.circuit.SeqexecCircuit._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

// import scalaz._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class CircuitReaderSpec extends FlatSpec with Matchers with PropertyChecks with ArbitrariesWebClient {
  // import edu.gemini.seqexec.model.SharedModelArbitraries._

  "CircuitReaderSpec" should
    "maintain reference equality for constant readers" in {
      (webSocketFocusRW === webSocketFocusRW.value) should be(true)
      (initialSyncFocusRW === initialSyncFocusRW.value) should be(true)
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
      forAll{ (i: SequenceId) =>
        (sequenceReader(i) === sequenceReader(i).value) should be(true)
      }
    }
}
