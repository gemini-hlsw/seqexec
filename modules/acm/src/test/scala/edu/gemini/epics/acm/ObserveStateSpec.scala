// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.epics.acm

import edu.gemini.epics.EpicsReader
import edu.gemini.epics.EpicsWriter
import edu.gemini.epics.api.ChannelListener
import edu.gemini.epics.ReadWriteClientEpicsChannel
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import java.lang.{Integer => JInteger}
import java.lang.{Short => JShort}
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
@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.PublicInference", "org.wartremover.warts.IsInstanceOf", "org.wartremover.warts.AsInstanceOf"))
final class ObserveStateSpec extends FunSuite with MockFactory {

  test("NIFS normal observation") {
    val epicsReader = mock[EpicsReader]
    val epicsWriter = mock[EpicsWriter]

    (epicsWriter.getEnumChannel _).expects("nifs:dc:nifsApply.DIR", *).returns(dirChannel)
    (epicsReader.getIntegerChannel _).expects("nifs:dc:nifsApply.VAL").returns(intChannelS)
    (epicsReader.getStringChannel _).expects("nifs:dc:nifsApply.MESS").returns(strChannel)
    (epicsReader.getIntegerChannel _).expects("nifs:dc:applyC.CLID").returns(intChannelS)
    (epicsReader.getEnumChannel _).expects("nifs:dc:applyC.VAL", *).returns(carChannel)
    (epicsReader.getStringChannel _).expects("nifs:dc:applyC.OMSS").returns(strChannel)
    (epicsReader.getIntegerChannel _).expects("nifs:dc:observeC.CLID").returns(intChannel)
    (epicsReader.getEnumChannel _).expects("nifs:dc:observeC.VAL", *).returns(carChannel)
    (epicsReader.getStringChannel _).expects("nifs:dc:observeC.OMSS").returns(strChannel)
    (epicsReader.getShortChannel _).expects("nifs:dc:stop.MARK").returns(shortChannel)
    (epicsReader.getShortChannel _).expects("nifs:dc:abort.MARK").returns(shortChannel)
    val observe = new CaObserveSenderImpl(
      "nifs::observeCmd",
      "nifs:dc:nifsApply",
      "nifs:dc:applyC",
      "nifs:dc:observeC",
      "nifs:dc:stop",
      "nifs:dc:abort",
      "NIFS Observe",
      classOf[CarState],
      epicsReader,
      epicsWriter)
    // Start idle
    assert(observe.applyState().isIdle)

    val observeErrorCount = new AtomicInteger()
    val observePauseCount = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l = observe.post()
    l.setCallback(new CaCommandListener() {
      def onFailure(ex: Exception): Unit = {
        observeErrorCount.incrementAndGet()
        ()
      }
      def onPause(): Unit = {
        observePauseCount.incrementAndGet()
        ()
      }
      def onSuccess(): Unit = {
        observeSuccessCount.incrementAndGet()
        ()
      }
    })

    // OBSERVE
    // OBSERVE goes BUSY
    // VAL change
    observe.onApplyValChange(359)
    assert(!observe.applyState().isIdle)
    // Observe CAR VAL change
    observe.onObserveCarValChange(CarState.BUSY)
    assert(!observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(359)
    assert(!observe.applyState().isIdle)
    // CAR VAL change
    observe.onCarValChange(CarState.BUSY)
    assert(!observe.applyState().isIdle)

    // OBSERVE goes IDLE
    // Observe CAR VAL change
    observe.onObserveCarValChange(CarState.IDLE)
    // And we are done and IDLE
    assert(observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(359)
    assert(observe.applyState().isIdle)
    // CAR VAL change
    observe.onCarValChange(CarState.IDLE)
    assert(observe.applyState().isIdle)
    l.waitDone(1, TimeUnit.SECONDS)
    assert(l.isDone)

    // Check that listener was called
    assert(observeErrorCount.get() === 0)
    assert(observePauseCount.get() === 0)
    assert(observeSuccessCount.get() === 1)
  }

  test("GMOS normal observation") {
    val epicsReader = mock[EpicsReader]
    val epicsWriter = mock[EpicsWriter]

    (epicsWriter.getEnumChannel _).expects("gm:apply.DIR", *).returns(dirChannel)
    (epicsReader.getIntegerChannel _).expects("gm:apply.VAL").returns(intChannelS)
    (epicsReader.getStringChannel _).expects("gm:apply.MESS").returns(strChannel)
    (epicsReader.getIntegerChannel _).expects("gm:applyC.CLID").returns(intChannelS)
    (epicsReader.getEnumChannel _).expects("gm:applyC.VAL", *).returns(carChannel)
    (epicsReader.getStringChannel _).expects("gm:applyC.OMSS").returns(strChannel)
    (epicsReader.getIntegerChannel _).expects("gm:dc:observeC.CLID").returns(intChannel)
    (epicsReader.getEnumChannel _).expects("gm:dc:observeC.VAL", *).returns(carChannel)
    (epicsReader.getStringChannel _).expects("gm:dc:observeC.OMSS").returns(strChannel)
    (epicsReader.getShortChannel _).expects("gm:stop.MARK").returns(shortChannel)
    (epicsReader.getShortChannel _).expects("gm:abort.MARK").returns(shortChannel)
    val observe = new CaObserveSenderImpl(
      "gmos::observeCmd",
      "gm:apply",
      "gm:applyC",
      "gm:dc:observeC",
      "gm:stop",
      "gm:abort",
      "GMOS Observe",
      classOf[CarState],
      epicsReader,
      epicsWriter)
    // Start idle
    assert(observe.applyState().isIdle)

    val observeErrorCount = new AtomicInteger()
    val observePauseCount = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l = observe.post()
    l.setCallback(new CaCommandListener() {
      def onFailure(ex: Exception): Unit = {
        observeErrorCount.incrementAndGet()
        ()
      }
      def onPause(): Unit = {
        observePauseCount.incrementAndGet()
        ()
      }
      def onSuccess(): Unit = {
        observeSuccessCount.incrementAndGet()
        ()
      }
    })

    // OBSERVE
    // OBSERVE goes BUSY
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
    // Observe CAR VAL change
    observe.onObserveCarValChange(CarState.BUSY)
    assert(!observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(4167)
    assert(!observe.applyState().isIdle)
    // Apply goes IDLE
    observe.onCarValChange(CarState.IDLE)
    assert(!observe.applyState().isIdle)

    // OBSERVE goes IDLE
    // Observe CAR VAL change
    observe.onObserveCarValChange(CarState.IDLE)
    // And we are done and IDLE
    assert(observe.applyState().isIdle)
    l.waitDone(1, TimeUnit.SECONDS)
    assert(l.isDone)

    // ENDOBSERVE
    // CAR CLID change
    observe.onApplyValChange(4168)
    assert(observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(4168)
    assert(observe.applyState().isIdle)
    // CAR VAL change
    observe.onCarValChange(CarState.BUSY)
    assert(observe.applyState().isIdle)

    observe.onApplyValChange(4168)
    assert(observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(4168)
    assert(observe.applyState().isIdle)
    // CAR VAL change
    observe.onCarValChange(CarState.IDLE)
    assert(observe.applyState().isIdle)

    // Check that listener was called
    assert(observeErrorCount.get() === 0)
    assert(observePauseCount.get() === 0)
    assert(observeSuccessCount.get() === 1)
  }

  test("GMOS paused observation") {
    val epicsReader = mock[EpicsReader]
    val epicsWriter = mock[EpicsWriter]

    (epicsWriter.getEnumChannel _).expects("gm:apply.DIR", *).returns(dirChannelPause)
    (epicsReader.getIntegerChannel _).expects("gm:apply.VAL").returns(intChannelS)
    (epicsReader.getStringChannel _).expects("gm:apply.MESS").returns(strChannel)
    (epicsReader.getIntegerChannel _).expects("gm:applyC.CLID").returns(intChannelS)
    (epicsReader.getEnumChannel _).expects("gm:applyC.VAL", *).returns(carChannel)
    (epicsReader.getStringChannel _).expects("gm:applyC.OMSS").returns(strChannel)
    (epicsReader.getIntegerChannel _).expects("gm:dc:observeC.CLID").returns(intChannel)
    (epicsReader.getEnumChannel _).expects("gm:dc:observeC.VAL", *).returns(carChannel)
    (epicsReader.getStringChannel _).expects("gm:dc:observeC.OMSS").returns(strChannel)
    (epicsReader.getShortChannel _).expects("gm:stop.MARK").returns(shortChannel)
    (epicsReader.getShortChannel _).expects("gm:abort.MARK").returns(shortChannel)

    val observe = new CaObserveSenderImpl(
      "gmos::observeCmd",
      "gm:apply",
      "gm:applyC",
      "gm:dc:observeC",
      "gm:stop",
      "gm:abort",
      "GMOS Observe",
      classOf[CarState],
      epicsReader,
      epicsWriter)
    // Start idle
    assert(observe.applyState().isIdle)
    val observeErrorCount = new AtomicInteger()
    val observePauseCount = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()

    // Post an observe
    val l = observe.post()
    l.setCallback(new CaCommandListener() {
      def onFailure(ex: Exception): Unit = {
        observeErrorCount.incrementAndGet()
        ()
      }
      def onPause(): Unit = {
        observePauseCount.incrementAndGet()
        ()
      }
      def onSuccess(): Unit = {
        observeSuccessCount.incrementAndGet()
        ()
      }
    })

    // OBSERVE
    // OBSERVE goes BUSY
    // VAL change
    observe.onApplyValChange(4170)
    assert(!observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(4170)
    assert(!observe.applyState().isIdle)
    // CAR VAL change
    observe.onCarValChange(CarState.BUSY)
    assert(!observe.applyState().isIdle)
    // Another VAL change
    observe.onApplyValChange(4170)
    assert(!observe.applyState().isIdle)
    // Observe CAR VAL change
    observe.onObserveCarValChange(CarState.BUSY)
    assert(!observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(4170)
    assert(!observe.applyState().isIdle)
    // Apply goes IDLE
    observe.onCarValChange(CarState.IDLE)
    assert(!observe.applyState().isIdle)

    // PAUSE observations
    observe.onApplyValChange(4171)
    assert(!observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(4171)
    assert(!observe.applyState().isIdle)
    // CAR VAL change
    observe.onCarValChange(CarState.BUSY)
    assert(!observe.applyState().isIdle)
    // Another VAL change
    observe.onApplyValChange(4171)
    assert(!observe.applyState().isIdle)
    // Observe CAR VAL change
    observe.onObserveCarValChange(CarState.PAUSED)
    // We are now idle
    assert(observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(4171)
    assert(observe.applyState().isIdle)
    // CAR VAL change
    observe.onCarValChange(CarState.IDLE)
    assert(observe.applyState().isIdle)
    assert(!l.isDone)
    // Check that listener was called
    assert(observeErrorCount.get() === 0)
    assert(observePauseCount.get() === 1)
    assert(observeSuccessCount.get() === 0)

    // Resume the observation
    val k = observe.post()
    k.setCallback(new CaCommandListener() {
      def onFailure(ex: Exception): Unit = {
        observeErrorCount.incrementAndGet()
        ()
      }
      def onPause(): Unit = {
        observePauseCount.incrementAndGet()
        ()
      }
      def onSuccess(): Unit = {
        observeSuccessCount.incrementAndGet()
        ()
      }
    })

    // Reset pause value
    observePauseCount.set(0)

    // RESUME OBSERVE
    observe.onApplyValChange(4172)
    // TODO Fix why this is idle here, it should be busy
    assert(!observe.applyState().isIdle)
    // Observe CAR VAL change
    observe.onObserveCarValChange(CarState.BUSY)
    assert(!observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(4172)
    assert(!observe.applyState().isIdle)
    // CAR VAL change
    observe.onCarValChange(CarState.BUSY)
    assert(!observe.applyState().isIdle)
    // Another VAL change
    observe.onApplyValChange(4172)
    assert(!observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(4172)
    assert(!observe.applyState().isIdle)
    // Apply goes IDLE
    observe.onCarValChange(CarState.IDLE)
    assert(!observe.applyState().isIdle)

    // Observe CAR VAL change
    observe.onObserveCarValChange(CarState.IDLE)
    assert(observe.applyState().isIdle)
    k.waitDone(1, TimeUnit.SECONDS)
    assert(k.isDone)

    // ENDOBSERVE
    // CAR CLID change
    observe.onApplyValChange(4173)
    assert(observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(4173)
    assert(observe.applyState().isIdle)
    // CAR VAL change
    observe.onCarValChange(CarState.BUSY)
    assert(observe.applyState().isIdle)

    observe.onApplyValChange(4173)
    assert(observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(4173)
    assert(observe.applyState().isIdle)
    // CAR VAL change
    observe.onCarValChange(CarState.IDLE)
    assert(observe.applyState().isIdle)

    // Check that listener was called
    assert(observeErrorCount.get() === 0)
    assert(observePauseCount.get() === 0)
    assert(observeSuccessCount.get() === 1)
  }

  test("NIRI normal observation") {
    val epicsReader = mock[EpicsReader]
    val epicsWriter = mock[EpicsWriter]

    (epicsWriter.getEnumChannel _).expects("niri:dc:apply.DIR", *).returns(dirChannel)
    (epicsReader.getIntegerChannel _).expects("niri:dc:apply.VAL").returns(intChannelS)
    (epicsReader.getStringChannel _).expects("niri:dc:apply.MESS").returns(strChannel)
    (epicsReader.getIntegerChannel _).expects("niri:dc:applyC.CLID").returns(intChannelS)
    (epicsReader.getEnumChannel _).expects("niri:dc:applyC.VAL", *).returns(carChannel)
    (epicsReader.getStringChannel _).expects("niri:dc:applyC.OMSS").returns(strChannel)
    (epicsReader.getIntegerChannel _).expects("niri:dc:observeC.CLID").returns(intChannel)
    (epicsReader.getEnumChannel _).expects("niri:dc:observeC.VAL", *).returns(carChannel)
    (epicsReader.getStringChannel _).expects("niri:dc:observeC.OMSS").returns(strChannel)
    (epicsReader.getShortChannel _).expects("niri:dc:stop.MARK").returns(shortChannel)
    (epicsReader.getShortChannel _).expects("niri:dc:abort.MARK").returns(shortChannel)

    val observe = new CaObserveSenderImpl(
      "niri::observeCmd",
      "niri:dc:apply",
      "niri:dc:applyC",
      "niri:dc:observeC",
      "niri:dc:stop",
      "niri:dc:abort",
      "NIRI Observe",
      classOf[CarState],
      epicsReader,
      epicsWriter)

    // Start idle
    assert(observe.applyState().isIdle)
    val observeErrorCount = new AtomicInteger()
    val observePauseCount = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l = observe.post()
    l.setCallback(new CaCommandListener() {
      def onFailure(ex: Exception): Unit = {
        observeErrorCount.incrementAndGet()
        ()
      }
      def onPause(): Unit = {
        observePauseCount.incrementAndGet()
        ()
      }
      def onSuccess(): Unit = {
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
    // CAR VAL change
    observe.onCarValChange(CarState.IDLE)
    assert(!observe.applyState().isIdle)

    // OBSERVE goes IDLE
    // Observe CAR VAL change
    observe.onObserveCarValChange(CarState.IDLE)
    // And we are done and IDLE
    assert(observe.applyState().isIdle)
    l.waitDone(1, TimeUnit.SECONDS)
    assert(l.isDone)

    // Check that listener was called
    assert(observeErrorCount.get() === 0)
    assert(observePauseCount.get() === 0)
    assert(observeSuccessCount.get() === 1)
  }

  test("GMOS observation with an error case 1") {
    val epicsReader = mock[EpicsReader]
    val epicsWriter = mock[EpicsWriter]

    (epicsWriter.getEnumChannel _).expects("gm:apply.DIR", *).returns(dirChannel)
    (epicsReader.getIntegerChannel _).expects("gm:apply.VAL").returns(intChannelS)
    (epicsReader.getStringChannel _).expects("gm:apply.MESS").returns(strChannel)
    (epicsReader.getIntegerChannel _).expects("gm:applyC.CLID").returns(intChannelS)
    (epicsReader.getEnumChannel _).expects("gm:applyC.VAL", *).returns(carChannel)
    (epicsReader.getStringChannel _).expects("gm:applyC.OMSS").returns(strChannel)
    (epicsReader.getIntegerChannel _).expects("gm:dc:observeC.CLID").returns(intChannel)
    (epicsReader.getEnumChannel _).expects("gm:dc:observeC.VAL", *).returns(carChannel)
    (epicsReader.getStringChannel _).expects("gm:dc:observeC.OMSS").returns(strChannelErr)
    (epicsReader.getShortChannel _).expects("gm:stop.MARK").returns(shortChannel)
    (epicsReader.getShortChannel _).expects("gm:abort.MARK").returns(shortChannel)
    val observe = new CaObserveSenderImpl(
      "gmos::observeCmd",
      "gm:apply",
      "gm:applyC",
      "gm:dc:observeC",
      "gm:stop",
      "gm:abort",
      "GMOS Observe",
      classOf[CarState],
      epicsReader,
      epicsWriter)
    // Start idle
    assert(observe.applyState().isIdle)

    val observeErrorCount = new AtomicInteger()
    val observePauseCount = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l = observe.post()
    l.setCallback(new CaCommandListener() {
      def onFailure(ex: Exception): Unit = {
        observeErrorCount.incrementAndGet()
        ()
      }
      def onPause(): Unit = {
        observePauseCount.incrementAndGet()
        ()
      }
      def onSuccess(): Unit = {
        observeSuccessCount.incrementAndGet()
        ()
      }
    })

    // OBSERVE
    // OBSERVE goes BUSY
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
    // Observe CAR VAL change
    observe.onObserveCarValChange(CarState.BUSY)
    assert(!observe.applyState().isIdle)
    // CAR CLID change
    observe.onCarClidChange(4167)
    assert(!observe.applyState().isIdle)
    // Apply goes IDLE
    observe.onCarValChange(CarState.IDLE)
    assert(!observe.applyState().isIdle)

    // OBSERVE goes ERROR
    // Observe CAR VAL change
    observe.onObserveCarValChange(CarState.ERROR)
    // We should capture the error and go IDLE
    assert(observe.applyState().isIdle)
    l.waitDone(1, TimeUnit.SECONDS)
    assert(l.isDone)

    // Check that listener was called
    assert(observeErrorCount.get() === 1)
    assert(observePauseCount.get() === 0)
    assert(observeSuccessCount.get() === 0)
  }

  test("GMOS observation with an error case 2") {
    val epicsReader = mock[EpicsReader]
    val epicsWriter = mock[EpicsWriter]

    (epicsWriter.getEnumChannel _).expects("gm:apply.DIR", *).returns(dirChannel)
    (epicsReader.getIntegerChannel _).expects("gm:apply.VAL").returns(intChannelS)
    (epicsReader.getStringChannel _).expects("gm:apply.MESS").returns(strChannel)
    (epicsReader.getIntegerChannel _).expects("gm:applyC.CLID").returns(intChannelS)
    (epicsReader.getEnumChannel _).expects("gm:applyC.VAL", *).returns(carChannel)
    (epicsReader.getStringChannel _).expects("gm:applyC.OMSS").returns(strChannel)
    (epicsReader.getIntegerChannel _).expects("gm:dc:observeC.CLID").returns(intChannel)
    (epicsReader.getEnumChannel _).expects("gm:dc:observeC.VAL", *).returns(carChannel)
    (epicsReader.getStringChannel _).expects("gm:dc:observeC.OMSS").returns(strChannelErr)
    (epicsReader.getShortChannel _).expects("gm:stop.MARK").returns(shortChannel)
    (epicsReader.getShortChannel _).expects("gm:abort.MARK").returns(shortChannel)
    val observe = new CaObserveSenderImpl(
      "gmos::observeCmd",
      "gm:apply",
      "gm:applyC",
      "gm:dc:observeC",
      "gm:stop",
      "gm:abort",
      "GMOS Observe",
      classOf[CarState],
      epicsReader,
      epicsWriter)
    // Start idle
    assert(observe.applyState().isIdle)

    val observeErrorCount = new AtomicInteger()
    val observePauseCount = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l = observe.post()
    l.setCallback(new CaCommandListener() {
      def onFailure(ex: Exception): Unit = {
        observeErrorCount.incrementAndGet()
        ()
      }
      def onPause(): Unit = {
        observePauseCount.incrementAndGet()
        ()
      }
      def onSuccess(): Unit = {
        observeSuccessCount.incrementAndGet()
        ()
      }
    })


    // OBSERVE
    // OBSERVE goes BUSY
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
    // Observe CAR goes directly to error
    observe.onObserveCarValChange(CarState.ERROR)
    // We should capture the error and go IDLE
    assert(observe.applyState().isIdle)
    l.waitDone(1, TimeUnit.SECONDS)
    assert(l.isDone)

    // Check that listener was called
    assert(observeErrorCount.get() === 1)
    assert(observePauseCount.get() === 0)
    assert(observeSuccessCount.get() === 0)
  }

  test("GSAOI normal observation") {
    val epicsReader = mock[EpicsReader]
    val epicsWriter = mock[EpicsWriter]

    (epicsWriter.getEnumChannel _).expects("gsaoi:dc:obsapply.DIR", *).returns(dirChannel)
    (epicsReader.getIntegerChannel _).expects("gsaoi:dc:obsapply.VAL").returns(intChannelS)
    (epicsReader.getStringChannel _).expects("gsaoi:dc:obsapply.MESS").returns(strChannel)
    (epicsReader.getIntegerChannel _).expects("gsaoi:dc:observeC.CLID").returns(intChannelS)
    (epicsReader.getEnumChannel _).expects("gsaoi:dc:observeC.VAL", *).returns(carChannel)
    (epicsReader.getStringChannel _).expects("gsaoi:dc:observeC.OMSS").returns(strChannel)
    (epicsReader.getShortChannel _).expects("gsaoi:dc:stop.MARK").returns(shortChannel)
    (epicsReader.getShortChannel _).expects("gsaoi:dc:abort.MARK").returns(shortChannel)
    val observe = new CaSimpleObserveSenderImpl(
      "gsaoi::observeCmd",
      "gsaoi:dc:obsapply",
      "gsaoi:dc:observeC",
      "gsaoi:dc:stop",
      "gsaoi:dc:abort",
      "GSAOI Observe",
      classOf[CarState],
      epicsReader,
      epicsWriter)
    // Start idle
    assert(observe.applyState().isIdle)

    val observeErrorCount = new AtomicInteger()
    val observePauseCount = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l = observe.post()
    l.setCallback(new CaCommandListener() {
      def onFailure(ex: Exception): Unit = {
        observeErrorCount.incrementAndGet()
        ()
      }
      def onPause(): Unit = {
        observePauseCount.incrementAndGet()
        ()
      }
      def onSuccess(): Unit = {
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
    // Apply goes IDLE
    observe.onCarValChange(CarState.IDLE)
    // And we are done and IDLE
    assert(observe.applyState().isIdle)
    l.waitDone(1, TimeUnit.SECONDS)
    assert(l.isDone)

    // Check that listener was called
    assert(observeErrorCount.get() === 0)
    assert(observePauseCount.get() === 0)
    assert(observeSuccessCount.get() === 1)
  }

  test("GSAOI stopped observation") {
    val epicsReader = mock[EpicsReader]
    val epicsWriter = mock[EpicsWriter]

    (epicsWriter.getEnumChannel _).expects("gsaoi:dc:obsapply.DIR", *).returns(dirChannel)
    (epicsReader.getIntegerChannel _).expects("gsaoi:dc:obsapply.VAL").returns(intChannelS)
    (epicsReader.getStringChannel _).expects("gsaoi:dc:obsapply.MESS").returns(strChannel)
    (epicsReader.getIntegerChannel _).expects("gsaoi:dc:observeC.CLID").returns(intChannelS)
    (epicsReader.getEnumChannel _).expects("gsaoi:dc:observeC.VAL", *).returns(carChannel)
    (epicsReader.getStringChannel _).expects("gsaoi:dc:observeC.OMSS").returns(strChannel)
    (epicsReader.getShortChannel _).expects("gsaoi:dc:stop.MARK").returns(shortChannel)
    (epicsReader.getShortChannel _).expects("gsaoi:dc:abort.MARK").returns(shortChannel)
    val observe = new CaSimpleObserveSenderImpl(
      "gsaoi::observeCmd",
      "gsaoi:dc:obsapply",
      "gsaoi:dc:observeC",
      "gsaoi:dc:stop",
      "gsaoi:dc:abort",
      "GSAOI Observe",
      classOf[CarState],
      epicsReader,
      epicsWriter)
    // Start idle
    assert(observe.applyState().isIdle)

    val observeErrorCount = new AtomicInteger()
    val observePauseCount = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l = observe.post()
    l.setCallback(new CaCommandListener() {
      def onFailure(ex: Exception): Unit = {
        observeErrorCount.incrementAndGet()
        ()
      }
      def onPause(): Unit = {
        observePauseCount.incrementAndGet()
        ()
      }
      def onSuccess(): Unit = {
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
    observe.onStopMarkChange(1.toShort)
    assert(!observe.applyState().isIdle)
    observe.onStopMarkChange(2.toShort)
    assert(!observe.applyState().isIdle)
    observe.onStopMarkChange(0.toShort)
    assert(!observe.applyState().isIdle)
    // Apply goes IDLE
    observe.onCarValChange(CarState.IDLE)
    // And we are done and IDLE
    assert(observe.applyState().isIdle)
    l.waitDone(1, TimeUnit.SECONDS)
    assert(l.error.isInstanceOf[CaObserveStopped])
    // Check that listener was called
    assert(observeErrorCount.get() === 1)
    assert(observePauseCount.get() === 0)
    assert(observeSuccessCount.get() === 0)
  }

  test("GSAOI aborted observation") {
    val epicsReader = mock[EpicsReader]
    val epicsWriter = mock[EpicsWriter]

    (epicsWriter.getEnumChannel _).expects("gsaoi:dc:obsapply.DIR", *).returns(dirChannel)
    (epicsReader.getIntegerChannel _).expects("gsaoi:dc:obsapply.VAL").returns(intChannelS)
    (epicsReader.getStringChannel _).expects("gsaoi:dc:obsapply.MESS").returns(strChannel)
    (epicsReader.getIntegerChannel _).expects("gsaoi:dc:observeC.CLID").returns(intChannelS)
    (epicsReader.getEnumChannel _).expects("gsaoi:dc:observeC.VAL", *).returns(carChannel)
    (epicsReader.getStringChannel _).expects("gsaoi:dc:observeC.OMSS").returns(strChannel)
    (epicsReader.getShortChannel _).expects("gsaoi:dc:stop.MARK").returns(shortChannel)
    (epicsReader.getShortChannel _).expects("gsaoi:dc:abort.MARK").returns(shortChannel)
    val observe = new CaSimpleObserveSenderImpl(
      "gsaoi::observeCmd",
      "gsaoi:dc:obsapply",
      "gsaoi:dc:observeC",
      "gsaoi:dc:stop",
      "gsaoi:dc:abort",
      "GSAOI Observe",
      classOf[CarState],
      epicsReader,
      epicsWriter)
    // Start idle
    assert(observe.applyState().isIdle)

    val observeErrorCount = new AtomicInteger()
    val observePauseCount = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l = observe.post()
    l.setCallback(new CaCommandListener() {
      def onFailure(ex: Exception): Unit = {
        observeErrorCount.incrementAndGet()
        ()
      }
      def onPause(): Unit = {
        observePauseCount.incrementAndGet()
        ()
      }
      def onSuccess(): Unit = {
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
    observe.onAbortMarkChange(1.toShort)
    assert(!observe.applyState().isIdle)
    observe.onAbortMarkChange(2.toShort)
    assert(!observe.applyState().isIdle)
    observe.onAbortMarkChange(0.toShort)
    assert(!observe.applyState().isIdle)
    // Apply goes IDLE
    observe.onCarValChange(CarState.IDLE)
    // And we are done and IDLE
    l.waitDone(1, TimeUnit.SECONDS)
    assert(observe.applyState().isIdle)
    assert(l.error.isInstanceOf[CaObserveAborted])

    // Check that listener was called
    assert(observeErrorCount.get() === 1)
    assert(observePauseCount.get() === 0)
    assert(observeSuccessCount.get() === 0)
  }


  // Functions to setup mocks with different expectations

  def dirChannel = {
    val m  = mock[CadDirectiveChannelMock]
    (m.setValue(_: CadDirective)).expects(*)
    m
  }

  def dirChannelPause = {
    val m  = mock[CadDirectiveChannelMock]//(dirCAJChannel, null, 0.1)
    (m.setValue(_: CadDirective)).expects(*).twice()
    m
  }

  def dirChannelNoSet = mock[CadDirectiveChannelMock]

  def carChannel = {
    val m  = mock[CarStateChannelMock]
    (m.registerListener(_: edu.gemini.epics.api.ChannelListener[CarState])).expects(*)
    m
  }

  def intChannelS = {
    val m  = mock[IntChannelMock]
    (m.registerListener(_: ChannelListener[JInteger])).expects(*)
    m
  }
  def intChannel =  mock[IntChannelMock]

  def strChannel = mock[StringChannelMock]

  def strChannelErr = {
    val m  = mock[StringChannelMock]
    (m.getFirst _).expects().once().returns("Error msg")
    m
  }

  def shortChannel = {
    val m  = mock[ShortChannelMock]
    (m.registerListener(_: ChannelListener[JShort])).expects(*)
    m
  }
}

// ScalaMock has a restriction that doesn't allow it to mock overloaded methods
// if the include a type param of an interface.
// This is exactly the case for `ReadWriteClientEpicsChannel[T]` and the [register|unRegister]Listener methods
// The workaround is to create classes such as below with concrete type and mock those
// https://github.com/paulbutcher/ScalaMock/issues/193
class CadDirectiveChannelMock extends ReadWriteClientEpicsChannel[CadDirective] {
  override def setValue(a: java.util.List[CadDirective]): Unit = {}
  override def setValue(a : CadDirective): Unit = {}
  override def getAll(): java.util.List[CadDirective] = null
  override def getDBR(): gov.aps.jca.dbr.DBR = null
  override def getFirst(): CadDirective = null.asInstanceOf[CadDirective]
  override def getName(): String = null
  override def getType(): gov.aps.jca.dbr.DBRType = null
  override def isValid(): Boolean = true
  override def registerListener(x: edu.gemini.epics.api.ChannelAlarmListener[CadDirective]): Unit = {}
  override def registerListener(x: edu.gemini.epics.api.ChannelListener[CadDirective]): Unit = {}
  override def unRegisterListener(x: edu.gemini.epics.api.ChannelAlarmListener[CadDirective]): Unit = {}
  override def unRegisterListener(x: edu.gemini.epics.api.ChannelListener[CadDirective]): Unit = {}
  override def destroy(): Unit = {}
}

class CarStateChannelMock extends ReadWriteClientEpicsChannel[CarState] {
  override def setValue(a: java.util.List[CarState]): Unit = {}
  override def setValue(a : CarState): Unit = {}
  override def getAll(): java.util.List[CarState] = null
  override def getDBR(): gov.aps.jca.dbr.DBR = null
  override def getFirst(): CarState = null.asInstanceOf[CarState]
  override def getName(): String = null
  override def getType(): gov.aps.jca.dbr.DBRType = null
  override def isValid(): Boolean = true
  override def registerListener(x: edu.gemini.epics.api.ChannelAlarmListener[CarState]): Unit = {}
  override def registerListener(x: edu.gemini.epics.api.ChannelListener[CarState]): Unit = {}
  override def unRegisterListener(x: edu.gemini.epics.api.ChannelAlarmListener[CarState]): Unit = {}
  override def unRegisterListener(x: edu.gemini.epics.api.ChannelListener[CarState]): Unit = {}
  override def destroy(): Unit = {}
}

class IntChannelMock extends ReadWriteClientEpicsChannel[JInteger] {
  override def setValue(a: java.util.List[JInteger]): Unit = {}
  override def setValue(a : JInteger): Unit = {}
  override def getAll(): java.util.List[JInteger] = null
  override def getDBR(): gov.aps.jca.dbr.DBR = null
  override def getFirst(): JInteger= null.asInstanceOf[JInteger]
  override def getName(): String = null
  override def getType(): gov.aps.jca.dbr.DBRType = null
  override def isValid(): Boolean = true
  override def registerListener(x: edu.gemini.epics.api.ChannelAlarmListener[JInteger]): Unit = {}
  override def registerListener(x: edu.gemini.epics.api.ChannelListener[JInteger]): Unit = {}
  override def unRegisterListener(x: edu.gemini.epics.api.ChannelAlarmListener[JInteger]): Unit = {}
  override def unRegisterListener(x: edu.gemini.epics.api.ChannelListener[JInteger]): Unit = {}
  override def destroy(): Unit = {}
}

class ShortChannelMock extends ReadWriteClientEpicsChannel[JShort] {
  override def setValue(a: java.util.List[JShort]): Unit = {}
  override def setValue(a : JShort): Unit = {}
  override def getAll(): java.util.List[JShort] = null
  override def getDBR(): gov.aps.jca.dbr.DBR = null
  override def getFirst(): JShort= null.asInstanceOf[JShort]
  override def getName(): String = null
  override def getType(): gov.aps.jca.dbr.DBRType = null
  override def isValid(): Boolean = true
  override def registerListener(x: edu.gemini.epics.api.ChannelAlarmListener[JShort]): Unit = {}
  override def registerListener(x: edu.gemini.epics.api.ChannelListener[JShort]): Unit = {}
  override def unRegisterListener(x: edu.gemini.epics.api.ChannelAlarmListener[JShort]): Unit = {}
  override def unRegisterListener(x: edu.gemini.epics.api.ChannelListener[JShort]): Unit = {}
  override def destroy(): Unit = {}
}

class StringChannelMock extends ReadWriteClientEpicsChannel[String] {
  override def setValue(a: java.util.List[String]): Unit = {}
  override def setValue(a : String): Unit = {}
  override def getAll(): java.util.List[String] = null
  override def getDBR(): gov.aps.jca.dbr.DBR = null
  override def getFirst(): String = null.asInstanceOf[String]
  override def getName(): String = null
  override def getType(): gov.aps.jca.dbr.DBRType = null
  override def isValid(): Boolean = true
  override def registerListener(x: edu.gemini.epics.api.ChannelAlarmListener[String]): Unit = {}
  override def registerListener(x: edu.gemini.epics.api.ChannelListener[String]): Unit = {}
  override def unRegisterListener(x: edu.gemini.epics.api.ChannelAlarmListener[String]): Unit = {}
  override def unRegisterListener(x: edu.gemini.epics.api.ChannelListener[String]): Unit = {}
  override def destroy(): Unit = {}
}
