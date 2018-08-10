// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import gem.Observation
import seqexec.web.client.circuit._
import seqexec.web.client.circuit.SeqexecCircuit._
import org.scalatest.prop.PropertyChecks

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
final class CircuitReaderSpec extends CatsSuite with PropertyChecks with ArbitrariesWebClient {
  checkAll("Eq[SequenceTabContentFocus]", EqTests[SequenceTabContentFocus].eqv)

  test("maintain reference equality for constant readers") {
      (webSocketFocusRW === webSocketFocusRW.value) should be(true)
      (initialSyncFocusRW === initialSyncFocusRW.value) should be(true)
      (tableStateRW === tableStateRW.value) should be(true)
      (statusAndLoadedSequencesReader === statusAndLoadedSequencesReader.value) should be(true)
      (statusReader === statusReader.value) should be(true)
      (sequenceInConflictReader === sequenceInConflictReader.value) should be(true)
      (headerSideBarReader === headerSideBarReader.value) should be(true)
      (logDisplayedReader === logDisplayedReader.value) should be(true)
      (availableTabs === availableTabs.value) should be(true)
      (sequenceTabs === sequenceTabs.value) should be(true)
      (configTableState === configTableState.value) should be(true)
    }
  test("maintain reference equality for id based readers") {
    forAll{ (i: Observation.Id) =>
      (sequenceTab(i) === sequenceTab(i).value) should be(true)
      (sequenceObserverReader(i) === sequenceObserverReader(i).value) should be(true)
      (statusAndStepReader(i) === statusAndStepReader(i).value) should be(true)
      (stepsTableReaderF(i) === stepsTableReaderF(i).value) should be(true)
      (stepsTableReader(i) === stepsTableReader(i).value) should be(true)
      (sequenceControlReader(i) === sequenceControlReader(i).value) should be(true)
      (sequenceReader(i) === sequenceReader(i).value) should be(true)
    }
  }
}
