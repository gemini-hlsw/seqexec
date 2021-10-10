// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.epics.acm

import edu.gemini.epics.api.ChannelListener
import edu.gemini.epics.{ EpicsReader, EpicsWriter }
import org.scalatest.funsuite.AnyFunSuite
import org.scalamock.scalatest.MockFactory

import java.util.concurrent.{ ScheduledExecutorService, ScheduledThreadPoolExecutor, TimeUnit }
import java.util.concurrent.atomic.AtomicInteger
import java.lang.{ Integer => JInteger }
import java.lang.{ Short => JShort }
import java.time.{ Duration, Instant }

final class ApplySenderSpec extends AnyFunSuite {
  import ApplySenderSpec._

  val executor: ScheduledExecutorService = new ScheduledThreadPoolExecutor(2)

  val applyTimeout: Duration = Duration.ofSeconds(2)

  test("Normal command sequence") {

    val (reader, writer) = ApplyMock.genericApplyMock("dummy:apply", "dummy:applyC")

    val apply: CaApplySenderImpl[CarState] =
      new CaApplySenderImpl[CarState]("apply",
                                      "dummy:apply",
                                      "dummy:applyC",
                                      "dummy apply",
                                      classOf[CarState],
                                      reader,
                                      writer,
                                      executor,
                                      TimestampProvider.Default
      )

    assert(!apply.isActive)

    val observeErrorCount   = new AtomicInteger()
    val observePauseCount   = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l                   = apply.postCallback(new CaCommandListener() {
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

    assert(!l.isDone)
    apply.onApplyValChange(100, Instant.ofEpochMilli(0))
    assert(!l.isDone)
    apply.onCarClidChange(100, Instant.ofEpochMilli(10))
    assert(!l.isDone)
    apply.onCarValChange(CarState.BUSY, Instant.ofEpochMilli(20))
    assert(!l.isDone)
    apply.onApplyValChange(100, Instant.ofEpochMilli(200))
    assert(!l.isDone)
    apply.onCarClidChange(100, Instant.ofEpochMilli(210))
    assert(!l.isDone)
    // The timestamp used verifies that the window is applied correctly.
    apply.onCarValChange(
      CarState.IDLE,
      Instant.ofEpochMilli(20).plus(CaApplySenderImpl.CompletionEventWindow.multipliedBy(2))
    )

    l.waitDone(1, TimeUnit.SECONDS)
    assert(l.isDone)

    assert(observeErrorCount.get == 0)
    assert(observePauseCount.get == 0)
    assert(observeSuccessCount.get == 1)

  }

  test("Command that fails when triggered") {

    val (reader, writer) = ApplyMock.genericApplyMock("dummy:apply", "dummy:applyC")

    val apply: CaApplySenderImpl[CarState] =
      new CaApplySenderImpl[CarState]("apply",
                                      "dummy:apply",
                                      "dummy:applyC",
                                      "dummy apply",
                                      classOf[CarState],
                                      reader,
                                      writer,
                                      executor,
                                      TimestampProvider.Default
      )

    assert(!apply.isActive)

    val observeErrorCount   = new AtomicInteger()
    val observePauseCount   = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l                   = apply.postCallback(new CaCommandListener() {
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

    assert(!l.isDone)
    apply.onApplyValChange(-1, Instant.ofEpochMilli(0))

    l.waitInactive(1, TimeUnit.SECONDS)

    assert(observeErrorCount.get == 1)
    assert(observePauseCount.get == 0)
    assert(observeSuccessCount.get == 0)

  }

  test("Command that fails after starting") {

    val (reader, writer) = ApplyMock.genericApplyMock("dummy:apply", "dummy:applyC")

    val apply: CaApplySenderImpl[CarState] =
      new CaApplySenderImpl[CarState]("apply",
                                      "dummy:apply",
                                      "dummy:applyC",
                                      "dummy apply",
                                      classOf[CarState],
                                      reader,
                                      writer,
                                      executor,
                                      TimestampProvider.Default
      )

    assert(!apply.isActive)

    val observeErrorCount   = new AtomicInteger()
    val observePauseCount   = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l                   = apply.postCallback(new CaCommandListener() {
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

    assert(!l.isDone)
    apply.onApplyValChange(100, Instant.ofEpochMilli(0))
    assert(!l.isDone)
    apply.onCarClidChange(100, Instant.ofEpochMilli(10))
    assert(!l.isDone)
    apply.onCarValChange(CarState.BUSY, Instant.ofEpochMilli(20))
    assert(!l.isDone)
    apply.onApplyValChange(100, Instant.ofEpochMilli(200))
    assert(!l.isDone)
    apply.onCarClidChange(100, Instant.ofEpochMilli(210))
    assert(!l.isDone)
    // The timestamp used verifies that the window is applied correctly.
    apply.onCarValChange(
      CarState.ERROR,
      Instant.ofEpochMilli(20).plus(CaApplySenderImpl.CompletionEventWindow.multipliedBy(2))
    )

    l.waitInactive(1, TimeUnit.SECONDS)

    assert(observeErrorCount.get == 1)
    assert(observePauseCount.get == 0)
    assert(observeSuccessCount.get == 0)

  }

  test("Successful abnormal command sequence 1") {

    val (reader, writer) = ApplyMock.genericApplyMock("dummy:apply", "dummy:applyC")

    val apply: CaApplySenderImpl[CarState] =
      new CaApplySenderImpl[CarState]("apply",
                                      "dummy:apply",
                                      "dummy:applyC",
                                      "dummy apply",
                                      classOf[CarState],
                                      reader,
                                      writer,
                                      executor,
                                      TimestampProvider.Default
      )

    assert(!apply.isActive)

    val observeErrorCount   = new AtomicInteger()
    val observePauseCount   = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l                   = apply.postCallback(new CaCommandListener() {
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

    assert(!l.isDone)
    apply.onApplyValChange(100, Instant.ofEpochMilli(10))
    assert(!l.isDone)
    apply.onCarValChange(CarState.BUSY, Instant.ofEpochMilli(20))
    assert(!l.isDone)
    apply.onApplyValChange(100, Instant.ofEpochMilli(30))
    assert(!l.isDone)
    apply.onCarValChange(CarState.IDLE, Instant.ofEpochMilli(40))
    assert(!l.isDone)
    apply.onCarClidChange(
      100,
      Instant.ofEpochMilli(40).plus(CaApplySenderImpl.CompletionEventWindow.dividedBy(2))
    )

    l.waitDone(2, TimeUnit.SECONDS)
    assert(l.isDone)

    assert(observeErrorCount.get == 0)
    assert(observePauseCount.get == 0)
    assert(observeSuccessCount.get == 1)

  }

  test("Successful abnormal command sequence 2") {

    val (reader, writer) = ApplyMock.genericApplyMock("dummy:apply", "dummy:applyC")

    val apply: CaApplySenderImpl[CarState] =
      new CaApplySenderImpl[CarState]("apply",
                                      "dummy:apply",
                                      "dummy:applyC",
                                      "dummy apply",
                                      classOf[CarState],
                                      reader,
                                      writer,
                                      executor,
                                      TimestampProvider.Default
      )

    assert(!apply.isActive)

    val observeErrorCount   = new AtomicInteger()
    val observePauseCount   = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l                   = apply.postCallback(new CaCommandListener() {
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

    assert(!l.isDone)
    apply.onCarClidChange(100, Instant.ofEpochMilli(20))
    assert(!l.isDone)
    apply.onCarValChange(CarState.BUSY, Instant.ofEpochMilli(30))
    assert(!l.isDone)
    apply.onCarClidChange(100, Instant.ofEpochMilli(50))
    assert(!l.isDone)
    apply.onCarValChange(CarState.IDLE, Instant.ofEpochMilli(60))
    assert(!l.isDone)
    apply.onApplyValChange(
      100,
      Instant.ofEpochMilli(60).plus(CaApplySenderImpl.CompletionEventWindow.dividedBy(2))
    )

    l.waitDone(2, TimeUnit.SECONDS)
    assert(l.isDone)

    assert(observeErrorCount.get == 0)
    assert(observePauseCount.get == 0)
    assert(observeSuccessCount.get == 1)

  }

  test("Failed abnormal command sequence 3") {

    val (reader, writer) = ApplyMock.genericApplyMock("dummy:apply", "dummy:applyC")

    val apply: CaApplySenderImpl[CarState] =
      new CaApplySenderImpl[CarState]("apply",
                                      "dummy:apply",
                                      "dummy:applyC",
                                      "dummy apply",
                                      classOf[CarState],
                                      reader,
                                      writer,
                                      executor,
                                      TimestampProvider.Default
      )
    apply.setTimeout(applyTimeout.toMillis, TimeUnit.MILLISECONDS)

    assert(!apply.isActive)

    val observeErrorCount   = new AtomicInteger()
    val observePauseCount   = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l                   = apply.postCallback(new CaCommandListener() {
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

    assert(!l.isDone)
    apply.onApplyValChange(100, Instant.ofEpochMilli(10))
    assert(!l.isDone)
    apply.onCarValChange(CarState.BUSY, Instant.ofEpochMilli(20))
    assert(!l.isDone)
    apply.onApplyValChange(100, Instant.ofEpochMilli(30))
    assert(!l.isDone)
    apply.onCarValChange(CarState.IDLE, Instant.ofEpochMilli(40))
    assert(!l.isDone)
    apply.onCarClidChange(
      100,
      Instant.ofEpochMilli(40).plus(CaApplySenderImpl.CompletionEventWindow.multipliedBy(2))
    )

    l.waitInactive(applyTimeout.getSeconds * 2, TimeUnit.SECONDS)

    assert(observeErrorCount.get == 1)
    assert(observePauseCount.get == 0)
    assert(observeSuccessCount.get == 0)

  }

  test("Failed abnormal command sequence 4") {

    val (reader, writer) = ApplyMock.genericApplyMock("dummy:apply", "dummy:applyC")

    val apply: CaApplySenderImpl[CarState] =
      new CaApplySenderImpl[CarState]("apply",
                                      "dummy:apply",
                                      "dummy:applyC",
                                      "dummy apply",
                                      classOf[CarState],
                                      reader,
                                      writer,
                                      executor,
                                      TimestampProvider.Default
      )
    apply.setTimeout(applyTimeout.toMillis, TimeUnit.MILLISECONDS)

    assert(!apply.isActive)

    val observeErrorCount   = new AtomicInteger()
    val observePauseCount   = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l                   = apply.postCallback(new CaCommandListener() {
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

    assert(!l.isDone)
    apply.onCarClidChange(100, Instant.ofEpochMilli(20))
    assert(!l.isDone)
    apply.onCarValChange(CarState.BUSY, Instant.ofEpochMilli(30))
    assert(!l.isDone)
    apply.onCarClidChange(100, Instant.ofEpochMilli(50))
    assert(!l.isDone)
    apply.onCarValChange(CarState.IDLE, Instant.ofEpochMilli(60))
    assert(!l.isDone)
    // change in apply.VAL arrives too late...
    apply.onApplyValChange(
      100,
      Instant.ofEpochMilli(60).plus(CaApplySenderImpl.CompletionEventWindow.multipliedBy(2))
    )

    l.waitInactive(applyTimeout.getSeconds * 2, TimeUnit.SECONDS)

    assert(observeErrorCount.get == 1)
    assert(observePauseCount.get == 0)
    assert(observeSuccessCount.get == 0)

  }
  test("Successful abnormal command sequence 5") {

    val (reader, writer) = ApplyMock.genericApplyMock("dummy:apply", "dummy:applyC")

    val apply: CaApplySenderImpl[CarState] =
      new CaApplySenderImpl[CarState]("apply",
                                      "dummy:apply",
                                      "dummy:applyC",
                                      "dummy apply",
                                      classOf[CarState],
                                      reader,
                                      writer,
                                      executor,
                                      TimestampProvider.Default
      )
    apply.setTimeout(applyTimeout.toMillis, TimeUnit.MILLISECONDS)

    assert(!apply.isActive)

    val observeErrorCount   = new AtomicInteger()
    val observePauseCount   = new AtomicInteger()
    val observeSuccessCount = new AtomicInteger()
    // Post an observe
    val l                   = apply.postCallback(new CaCommandListener() {
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

    // The command control sees the end of a previous command
    assert(!l.isDone)
    apply.onCarValChange(CarState.BUSY, Instant.ofEpochMilli(10))
    assert(!l.isDone)
    apply.onCarClidChange(99, Instant.ofEpochMilli(20))
    assert(!l.isDone)
    apply.onCarValChange(CarState.IDLE, Instant.ofEpochMilli(30))
    assert(!l.isDone)
    apply.onApplyValChange(100,
                           Instant.ofEpochMilli(40).plus(CaApplySenderImpl.CompletionEventWindow)
    )
    assert(!l.isDone)
    apply.onCarClidChange(100,
                          Instant.ofEpochMilli(50).plus(CaApplySenderImpl.CompletionEventWindow)
    )
    assert(!l.isDone)
    apply.onCarValChange(CarState.BUSY,
                         Instant.ofEpochMilli(60).plus(CaApplySenderImpl.CompletionEventWindow)
    )
    assert(!l.isDone)
    apply.onCarValChange(CarState.IDLE,
                         Instant.ofEpochMilli(70).plus(CaApplySenderImpl.CompletionEventWindow)
    )

    l.waitDone(applyTimeout.getSeconds * 2, TimeUnit.SECONDS)
    assert(l.isDone)

    assert(observeErrorCount.get == 0)
    assert(observePauseCount.get == 0)
    assert(observeSuccessCount.get == 1)

  }

}

object ApplySenderSpec {

  object ApplyMock extends MockFactory {
    def genericApplyMock(
      apply:    String,
      applyCar: String
    ): (EpicsReader, EpicsWriter) = {
      val epicsReader: EpicsReader = mock[EpicsReader]
      val epicsWriter: EpicsWriter = mock[EpicsWriter]

      (epicsWriter.getEnumChannel[CadDirective] _).expects(s"$apply.DIR", *).returns(dirChannel)
      (epicsReader.getIntegerChannel _).expects(s"$apply.VAL").returns(intChannelS)
      (epicsReader.getStringChannel _).expects(s"$apply.MESS").returns(strChannelErr)
      (epicsReader.getIntegerChannel _).expects(s"$applyCar.CLID").returns(intChannelS)
      (epicsReader.getEnumChannel[CarState] _).expects(s"$applyCar.VAL", *).returns(carChannel)
      (epicsReader.getStringChannel _).expects(s"$applyCar.OMSS").returns(strChannelErr)
      (epicsReader, epicsWriter)
    }

    def dirChannel: CadDirectiveChannelMock = {
      val m = mock[CadDirectiveChannelMock]
      (m.setValue(_: CadDirective)).expects(*)
      m
    }

    def dirChannelPause: CadDirectiveChannelMock = {
      val m = mock[CadDirectiveChannelMock] //(dirCAJChannel, null, 0.1)
      (m.setValue(_: CadDirective)).expects(*).twice()
      m
    }

    def dirChannelNoSet: CadDirectiveChannelMock = mock[CadDirectiveChannelMock]

    def carChannel: CarStateChannelMock = {
      val m = mock[CarStateChannelMock]
      (m.registerListener(_: edu.gemini.epics.api.ChannelListener[CarState])).expects(*)
      (m.getFirst _).expects().returns(CarState.IDLE)
      m
    }

    def intChannelS: IntChannelMock = {
      val m = mock[IntChannelMock]
      (m.registerListener(_: ChannelListener[JInteger])).expects(*)
      m
    }

    def intChannel: IntChannelMock = mock[IntChannelMock]

    def strChannel: StringChannelMock = mock[StringChannelMock]

    def strChannelErr: StringChannelMock = {
      val m = mock[StringChannelMock]
      (m.getFirst _).expects().once().returns("Error msg")
      m
    }

    def shortChannel: ShortChannelMock = {
      val m = mock[ShortChannelMock]
      (m.registerListener(_: ChannelListener[JShort])).expects(*)
      m
    }
  }

}
