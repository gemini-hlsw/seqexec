// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.epics.acm

import com.cosylab.epics.caj.CAJContext
import edu.gemini.epics.EpicsService
import org.scalamock.scalatest.MockFactory
import org.scalatest._

/**
  * Tests of the observe state machine
  *
  * We want to verify that the Apply state and Observe state FSM properly do
  * state transitions as the EPICS channels change.
  * To speed up the process the CAJContext is mocked as we don't really
  * care about the state of the channels. Instead we want to only observe
  * the state transitions
  */
@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.PublicInference"))
final class ObserveStateSpec extends FunSuite with MockFactory {

  test("NIFS normal observation") {
    val context: CAJContext = mock[CAJContext]
    (context.addContextExceptionListener _).expects(*).returns(()).repeat(5)
    (context.addContextMessageListener _).expects(*).returns(()).repeat(5)
    (context.pendIO _).expects(*).returns(()).repeat(1 to 6)
    // We just return null as we don't need the channels and don't want to mock them
    (context.createChannel(_: String)).expects("nifs:dc:nifsApply.DIR").returns(null)
    (context.createChannel(_: String)).expects("nifs:dc:nifsApply.VAL").returns(null)
    (context.createChannel(_: String)).expects("nifs:dc:nifsApply.MESS").returns(null)
    (context.createChannel(_: String)).expects("nifs:dc:applyC.CLID").returns(null)
    (context.createChannel(_: String)).expects("nifs:dc:applyC.VAL").returns(null)
    (context.createChannel(_: String)).expects("nifs:dc:applyC.OMSS").returns(null)
    val epicsService = new EpicsService(context)
    val observe = new CaObserveSenderImpl(
      "nifs::observeCmd",
      "nifs:dc:nifsApply",
      "nifs:dc:applyC",
      "nifs:dc:observeC",
      "nifs:dc:stop",
      "nifs:dc:abort",
      "NIFS Observe",
      classOf[CarState],
      epicsService)
    // Start idle
    assert(observe.applyState().isIdle())
    // Post an observe
    observe.post()

    // APPLY Goes to busy
    // VAL change
    observe.onApplyValChange(358)
    assert(!observe.applyState().isIdle())
    assert(!observe.observeState().isDone())
    // CAR CLID change
    observe.onCarClidChange(358)
    assert(!observe.applyState().isIdle())
    assert(!observe.observeState().isDone())
    // CAR VAL change
    observe.onCarValChange(CarState.BUSY)
    assert(!observe.applyState().isIdle())
    assert(!observe.observeState().isDone())

    // APPLY Goes to IDLE
    // Another VAL change
    observe.onApplyValChange(358)
    assert(!observe.applyState().isIdle())
    assert(!observe.observeState().isDone())
    // CAR CLID change
    observe.onCarClidChange(358)
    assert(!observe.applyState().isIdle())
    assert(!observe.observeState().isDone())
    // CAR VAL change
    observe.onCarValChange(CarState.IDLE)
    assert(!observe.applyState().isIdle())
    assert(!observe.observeState().isDone())

    // OBSERVE goes BUSY
    // VAL change
    observe.onApplyValChange(359)
    assert(!observe.applyState().isIdle())
    assert(!observe.observeState().isDone())
    // Observe CAR VAL change
    observe.onObserveCarValChange(CarState.BUSY)
    assert(!observe.applyState().isIdle())
    assert(!observe.observeState().isDone())
    // CAR CLID change
    observe.onCarClidChange(359)
    assert(!observe.applyState().isIdle())
    assert(!observe.observeState().isDone())
    // CAR VAL change
    observe.onCarValChange(CarState.BUSY)
    assert(!observe.applyState().isIdle())
    assert(!observe.observeState().isDone())

    // OBSERVE goes IDLE
    // Observe CAR VAL change
    observe.onObserveCarValChange(CarState.IDLE)
    // And we are done and IDLE
    assert(observe.applyState().isIdle())
    // CAR CLID change
    observe.onCarClidChange(359)
    assert(observe.applyState().isIdle())
    // CAR VAL change
    observe.onCarValChange(CarState.IDLE)
    assert(observe.applyState().isIdle())
  }

}
