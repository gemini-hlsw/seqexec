// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.epics.acm

import java.util.concurrent.{ ScheduledExecutorService, ScheduledThreadPoolExecutor, TimeUnit }
import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.funsuite.AnyFunSuite

/**
 * Tests of the observe state machine timeouts
 */
final class ObserveTimeoutSpec extends AnyFunSuite with NiriMocks with GsaoiMocks {

  val executor: ScheduledExecutorService = new ScheduledThreadPoolExecutor(2)

  test("NIRI no response should timeout") {
    val (epicsReader, epicsWriter) = niriMocks

    val observe = new CaObserveSenderImpl("niri::observeCmd",
                                          "niri:dc:apply",
                                          "niri:dc:applyC",
                                          "niri:dc:observeC",
                                          "niri:dc:stop",
                                          "niri:dc:abort",
                                          "NIRI Observe",
                                          classOf[CarState],
                                          epicsReader,
                                          epicsWriter,
                                          executor
    )

    observe.setTimeout(5, TimeUnit.SECONDS)
    // Start idle
    assert(observe.applyState().isIdle)
    val observeErrorCount   = new AtomicInteger()
    val observePauseCount   = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l                   = observe.post()
    l.setCallback(new CaCommandListener() {
      def onFailure(ex: Exception): Unit = {
        observeErrorCount.incrementAndGet()
        ()
      }
      def onPause(): Unit                = {
        observePauseCount.incrementAndGet()
        ()
      }
      def onSuccess(): Unit              = {
        observeSuccessCount.incrementAndGet()
        ()
      }
    })

    // OBSERVE
    // OBSERVE goes BUSY
    observe.onObserveCarValChange(CarState.BUSY)
    assert(!observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(365)
    assert(!observe.applyState().isIdle)
    // CAR VAL change
    observe.onCarValChange(CarState.BUSY)
    assert(!observe.applyState().isIdle)

    // Apply VAL change
    observe.onApplyValChange(365)
    assert(!observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(365)
    assert(!observe.applyState().isIdle)
    // OBSERVE never completes
    // observe.onObserveCarValChange(CarState.IDLE)
    // And we are done and IDLE
    assert(!observe.applyState().isIdle)
    // wait for the timeout
    l.waitDone(20, TimeUnit.SECONDS)
    assert(l.isDone)
    // Now we are idle
    assert(observe.applyState().isIdle)

    // Check that listener was called
    assert(observeErrorCount.get() === 1)
    assert(observePauseCount.get() === 0)
    assert(observeSuccessCount.get() === 0)
  }

  test("GSAOI apply no response should timeout") {

    val (epicsReader, epicsWriter) = gsaoiMocks
    val observe                    = new CaSimpleObserveSenderImpl("gsaoi::observeCmd",
                                                "gsaoi:dc:obsapply",
                                                "gsaoi:dc:observeC",
                                                "gsaoi:dc:stop",
                                                "gsaoi:dc:abort",
                                                "GSAOI Observe",
                                                classOf[CarState],
                                                epicsReader,
                                                epicsWriter,
                                                executor
    )
    observe.setTimeout(5, TimeUnit.SECONDS)
    // Start idle
    assert(observe.applyState().isIdle)

    val observeErrorCount   = new AtomicInteger()
    val observePauseCount   = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l                   = observe.post()
    l.setCallback(new CaCommandListener() {
      def onFailure(ex: Exception): Unit = {
        observeErrorCount.incrementAndGet()
        ()
      }
      def onPause(): Unit                = {
        observePauseCount.incrementAndGet()
        ()
      }
      def onSuccess(): Unit              = {
        observeSuccessCount.incrementAndGet()
        ()
      }
    })

    // VAL change
    observe.onApplyValChange(4167)
    assert(!observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(4167)
    assert(!observe.applyState().isIdle)
    // CAR VAL change
    observe.onCarValChange(CarState.BUSY)
    assert(!observe.applyState().isIdle)
    // Another VAL change
    observe.onApplyValChange(4167)
    assert(!observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(4167)
    assert(!observe.applyState().isIdle)
    // CAR never returs to IDLE
    // observe.onCarValChange(CarState.IDLE)
    // And we are done and IDLE
    assert(!observe.applyState().isIdle)
    l.waitDone(20, TimeUnit.SECONDS)
    assert(l.isDone)

    // Check that listener was called
    assert(observeErrorCount.get() === 1)
    assert(observePauseCount.get() === 0)
    assert(observeSuccessCount.get() === 0)
  }
}
