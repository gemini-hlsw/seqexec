// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
// import gem.Observation
// import monocle.law.discipline.LensTests
// import seqexec.model.QueueId
// import seqexec.model.SeqexecModelArbitraries.queueIdArb
// import seqexec.web.client.model._
import seqexec.web.client.circuit._
// import seqexec.web.client.circuit.SeqexecCircuit._

final class CircuitReaderSpec
    extends CatsSuite
    with ArbitrariesWebClient {

  checkAll("Eq[TabContentFocus]", EqTests[TabContentFocus].eqv)
  checkAll("Eq[SequenceTabContentFocus]", EqTests[SequenceTabContentFocus].eqv)
  checkAll("Eq[CalQueueTabContentFocus]", EqTests[CalQueueTabContentFocus].eqv)
  checkAll("Eq[SequencesFocus]", EqTests[SequencesFocus].eqv)
  checkAll("Eq[SequenceInfoFocus]", EqTests[SequenceInfoFocus].eqv)
  // checkAll("sequencesFocusL", LensTests(SequencesFocus.sequencesFocusL))
  // checkAll("sodLocationFocusL", LensTests(SODLocationFocus.sodLocationFocusL))
  // checkAll("initialSyncFocusL", LensTests(InitialSyncFocus.initialSyncFocusL))
  // checkAll("clientStatusFocusL", LensTests(ClientStatus.clientStatusFocusL))
  // checkAll("webSocketFocusL", LensTests(WebSocketsFocus.webSocketFocusL))
  // checkAll("tableStateL", LensTests(AppTableStates.tableStateL))

  // test("maintain reference equality for constant readers") {
    // (webSocketFocusRW === webSocketFocusRW.value) should be(true)
    // (initialSyncFocusRW === initialSyncFocusRW.value) should be(true)
    // (tableStateRW === tableStateRW.value) should be(true)
    // (sequencesReaderRW === sequencesReaderRW.value) should be(true)
    // (sodLocationReaderRW === sodLocationReaderRW.value) should be(true)
    // (statusAndLoadedSequencesReader === statusAndLoadedSequencesReader.value) should be(true)
    // (statusReader === statusReader.value) should be(true)
    // (headerSideBarReader === headerSideBarReader.value) should be(true)
    // (logDisplayedReader === logDisplayedReader.value) should be(true)
    // (tabsReader === tabsReader.value) should be(true)
    // (seqexecTabs === seqexecTabs.value) should be(true)
    // (queueOperationsRW === queueOperationsRW.value) should be(true)
    // (sequencesOnDisplayRW === sequencesOnDisplayRW.value) should be(true)
  //   (queueFocusRW === queueFocusRW.value) should be(true)
  // }
  // test("maintain reference equality for id based readers") {
  //   forAll { (i: Observation.Id) =>
  //     (sequenceTab(i) === sequenceTab(i).value) should be(true)
  //     (sequenceObserverReader(i) === sequenceObserverReader(i).value) should be(true)
  //     (statusAndStepReader(i) === statusAndStepReader(i).value) should be(true)
  //     (stepsTableReaderF(i) === stepsTableReaderF(i).value) should be(true)
  //     (stepsTableReader(i) === stepsTableReader(i).value) should be(true)
  //     (sequenceControlReader(i) === sequenceControlReader(i).value) should be(true)
  //   }
  // }
  // test("maintain reference equality for queue id based readers") {
  //   forAll { (i: QueueId) =>
  //     (calQueueControlReader(i) === calQueueControlReader(i).value) should be(true)
  //     (calQueueReader(i) === calQueueReader(i).value) should be(true)
  //   }
  // }
}
