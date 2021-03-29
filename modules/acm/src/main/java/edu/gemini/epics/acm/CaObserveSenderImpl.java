/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import edu.gemini.epics.EpicsReader;
import edu.gemini.epics.EpicsWriter;
import edu.gemini.epics.ReadOnlyClientEpicsChannel;
import edu.gemini.epics.api.ChannelListener;
import gov.aps.jca.CAException;
import gov.aps.jca.TimeoutException;

import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ThreadFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Created by jluhrs on 10/17/17.
 */
public class CaObserveSenderImpl<C extends Enum<C> & CarStateGeneric> implements ApplySenderWithResource {

    private static final Logger LOG = LoggerFactory.getLogger(CaObserveSenderImpl.class.getName());
    private static final String CAD_MARK_SUFFIX = ".MARK";

    private final String name;
    private final String description;

    private final CaApplyRecord apply;
    private final CaCarRecord<C> car;
    private final CaCarRecord<C> observeCar;
    private final ReadOnlyClientEpicsChannel<Short> abortMark;
    private final ReadOnlyClientEpicsChannel<Short> stopMark;

    private final short MRK_PRESET = 2;
    private final short MRK_IDLE = 0;

    private final Boolean trace = Boolean.getBoolean("epics.observe.trace");

    private long timeout;
    private TimeUnit timeoutUnit;
    private final ScheduledExecutorService executor;
    private ScheduledFuture<?> timeoutFuture;
    private final ChannelListener<Integer> valListener;
    private final ChannelListener<Integer> carClidListener;
    private final ChannelListener<C> carValListener;
    private final ChannelListener<C> observeCarValListener;
    private ChannelListener<Short> abortMarkListener;
    private ChannelListener<Short> stopMarkListener;
    private CaObserveSenderImpl.ApplyState currentState;

    public CaObserveSenderImpl(
        final String name,
        final String applyRecord,
        final String carRecord,
        final String observeCarRecord,
        final String stopCmd,
        final String abortCmd,
        final String description,
        final Class<C> carClass,
        final EpicsReader epicsReader,
        final EpicsWriter epicsWriter,
        final ScheduledExecutorService executor
    ) throws CAException {
        super();
        this.name = name;
        this.description = description;
        this.currentState = idleState;
        this.executor = executor;

        apply = new CaApplyRecord(applyRecord, epicsReader, epicsWriter);
        // apply.VAL int > 0
        apply.registerValListener(valListener = (String arg0, List<Integer> newVals) -> {
            if (newVals != null && !newVals.isEmpty()) {
                CaObserveSenderImpl.this.onApplyValChange(newVals.get(0));
            }
        });

        car = new CaCarRecord<C>(carRecord, carClass, epicsReader);
        // applyC.CLID int > 0
        car.registerClidListener(carClidListener = (String arg0, List<Integer> newVals) -> {
            if (newVals != null && !newVals.isEmpty()) {
                CaObserveSenderImpl.this.onCarClidChange(newVals.get(0));
            }
        });
        // applyC.VAL BUSY/IDLE
        car.registerValListener(carValListener = (String arg0, List<C> newVals) -> {
            if (newVals != null && !newVals.isEmpty()) {
                CaObserveSenderImpl.this.onCarValChange(newVals.get(0));
            }
        });

        observeCar = new CaCarRecord<C>(observeCarRecord, carClass, epicsReader);
        // observeC.VAL BUSY/IDLE/PAUSED
        observeCar.registerValListener(observeCarValListener = (String arg0, List<C> newVals) -> {
            if (newVals != null && !newVals.isEmpty()) {
                CaObserveSenderImpl.this.onObserveCarValChange(newVals.get(0));
            }
        });

        ReadOnlyClientEpicsChannel<Short> stopMark = null;
        if (stopCmd != null && stopCmd.length() > 0) {
            try {
                stopMark = epicsReader.getShortChannel(stopCmd + CAD_MARK_SUFFIX);
            } catch(Throwable e) {
                LOG.warn(e.getMessage());
            }
            if (stopMark != null) {
                stopMark.registerListener(stopMarkListener = (String arg0, List<Short> newVals) -> {
                    if (newVals != null && !newVals.isEmpty()) {
                        CaObserveSenderImpl.this.onStopMarkChange(newVals.get(0));
                    }
                });
            }
        } else {
            stopMark = null;
        }
        this.stopMark = stopMark;

        ReadOnlyClientEpicsChannel<Short> abortMark = null;
        if (abortCmd != null && abortCmd.length() > 0) {
            try {
                abortMark = epicsReader.getShortChannel(abortCmd + CAD_MARK_SUFFIX);
            } catch(Throwable e) {
                LOG.warn(e.getMessage());
            }
            if (abortMark != null) {
                abortMark.registerListener(abortMarkListener = (String arg0, List<Short> newVals) -> {
                    if (newVals != null && !newVals.isEmpty()) {
                        CaObserveSenderImpl.this.onAbortMarkChange(newVals.get(0));
                    }
                });
            }
        } else {
            abortMark = null;
        }
        this.abortMark = abortMark;

    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getApply() {
        return apply.getEpicsName();
    }

    @Override
    public String getCAR() {
        return car.getEpicsName();
    }

    @Override
    public void unbind() {

        executor.shutdown();

        try {
            apply.unregisterValListener(valListener);
        } catch (CAException e) {
            LOG.warn(e.getMessage());
        }
        try {
            car.unregisterClidListener(carClidListener);
        } catch (CAException e) {
            LOG.warn(e.getMessage());
        }
        try {
            car.unregisterValListener(carValListener);
        } catch (CAException e) {
            LOG.warn(e.getMessage());
        }
        try {
            observeCar.unregisterValListener(observeCarValListener);
        } catch (CAException e) {
            LOG.warn(e.getMessage());
        }
        if (stopMark != null) {
            try {
                stopMark.unRegisterListener(stopMarkListener);
            } catch (CAException e) {
                LOG.warn(e.getMessage());
            }
        }
        if (abortMark != null) {
            try {
                abortMark.unRegisterListener(abortMarkListener);
            } catch (CAException e) {
                LOG.warn(e.getMessage());
            }
        }

        apply.unbind();
        car.unbind();
    }

    @Override
    public synchronized CaCommandMonitor post() {
        CaCommandMonitorImpl cm = new CaCommandMonitorImpl();
        if (!currentState.equals(idleState)) {
            failCommand(cm, new CaCommandInProgress());
        } else {
            try {
                apply.setDir(CadDirective.START);
                currentState = new CaObserveSenderImpl.WaitApplyPreset(cm);
                if (timeout > 0) {
                    timeoutFuture = executor.schedule(() ->  CaObserveSenderImpl.this.onTimeout(), timeout, timeoutUnit);
                }
            } catch (CAException | TimeoutException e) {
                failCommand(cm, e);
            }
        }

        return cm;
    }

    @Override
    public CaCommandMonitor postWait() throws InterruptedException {
        CaCommandMonitor cm = post();
        cm.waitDone();
        return cm;
    }

    @Override
    public CaCommandMonitor postCallback(final CaCommandListener callback) {
        CaCommandMonitor cm = post();
        cm.setCallback(callback);
        return cm;
    }

    // Used on a FSM tracking the state of apply
    protected interface ApplyState {
        String signature();

        CaObserveSenderImpl.ApplyState onApplyValChange(final Integer val);

        CaObserveSenderImpl.ApplyState onCarValChange(final CarStateGeneric carState);

        CaObserveSenderImpl.ApplyState onCarClidChange(final Integer val);

        ObserveState currentObserveState();

        CaObserveSenderImpl.ApplyState copyWithObserveState(final ObserveState observeState);

        default CaObserveSenderImpl.ApplyState onStopMarkChange(final Short val) {
            return copyWithObserveState(currentObserveState().onStopMarkChange(val));
        }

        default CaObserveSenderImpl.ApplyState onAbortMarkChange(final Short val) {
            return copyWithObserveState(currentObserveState().onAbortMarkChange(val));
        }

        default CaObserveSenderImpl.ApplyState onObserveCarValChange(final CarStateGeneric carState) {
            // In case of error we go to idle
            if (carState.isError()) {
                return idleState;
            } else {
                return copyWithObserveState(currentObserveState().onObserveCarValChange(carState));
            }
        }

        default boolean isIdle() {
            return false;
        }

        CaObserveSenderImpl.ApplyState onTimeout();
    }

    // Used on a FSM tracking the state of observe
    protected interface ObserveState {
        String signature();

        ObserveState onObserveCarValChange(final CarStateGeneric carState);

        CaObserveSenderImpl.ObserveState onStopMarkChange(final Short val);

        CaObserveSenderImpl.ObserveState onAbortMarkChange(final Short val);

        default boolean isDone() {
            return false;
        }

        default boolean isStopped() {
            return false;
        }

        default boolean isAborted() {
            return false;
        }

        default boolean isPaused() {
            return false;
        }
    }

    // Initial state before listening for anything
    private final static class ObserveIdleState implements ObserveState {

        ObserveIdleState() { }

        public String signature() {return "ObserveIdle";}

        public ObserveState onObserveCarValChange(final CarStateGeneric carState) {
            return this;
        }

        public CaObserveSenderImpl.ObserveState onStopMarkChange(final Short val) {
            return this;
        }

        public CaObserveSenderImpl.ObserveState onAbortMarkChange(final Short val) {
            return this;
        }

    }

    private static final ObserveState idleObserveState = new ObserveIdleState();

    // At this state we are waiting for busy on the observeC
    private final class ObserveWaitBusy implements ObserveState {
        final CaCommandMonitorImpl cm;
        final CarStateGeneric observeCarState;

        ObserveWaitBusy(final CaCommandMonitorImpl cm) {
            this.observeCarState = null;
            this.cm = cm;
        }

        ObserveWaitBusy(final CarStateGeneric observeCarState, final CaCommandMonitorImpl cm) {
            this.observeCarState = observeCarState;
            this.cm = cm;
        }

        public String signature() { return "ObserveWaitBusy(observeCarState = " + observeCarState + ")"; }

        public ObserveState onObserveCarValChange(final CarStateGeneric carState) {
          if (carState.isBusy()) {
              // Now we are busy, let's go to wait for idle
              return new ObserveWaitIdle(carState, cm);
          } else if (carState.isError()) {
              // Signal an error
              failCommandWithObserveCarError(cm);
              return idleObserveState;
          } else if (carState.isPaused()) {
              return idleObserveState;
          } else {
              // No change, preserve the car
              return new ObserveWaitBusy(carState, cm);
          }
        }

        public CaObserveSenderImpl.ObserveState onStopMarkChange(final Short val) {
            return new ObserveWaitStop(cm, val);
        }

        public CaObserveSenderImpl.ObserveState onAbortMarkChange(final Short val) {
            return new ObserveWaitAbort(cm, val);
        }

    }

    // Here we wait for observeC to go to idle
    private final class ObserveWaitIdle implements ObserveState {
        final CarStateGeneric observeCarState;
        final CaCommandMonitorImpl cm;

        ObserveWaitIdle(final CarStateGeneric observeCarState, final CaCommandMonitorImpl cm) {
            this.observeCarState = observeCarState;
            this.cm = cm;
        }

        @Override
        public String signature() { return "ObserveWaitIdle(observeCarState = " + observeCarState + ")"; }

        @Override
        public ObserveState onObserveCarValChange(final CarStateGeneric carState) {
            if (observeCarState.isBusy() && carState.isIdle()) {
                return new ObserveDone(carState, cm);
            }
            else if (carState.isBusy()) {
                return new ObserveWaitIdle(carState, cm);
            }
            else if (carState.isError()) {
                failCommandWithObserveCarError(cm);
                return idleObserveState;
            }
            else if (carState.isPaused()) {
                pauseCommand(cm);
                return new ObservePaused(carState, cm);
            }
            else {
                // Should never get here
                return this;
            }
        }

        @Override
        public CaObserveSenderImpl.ObserveState onStopMarkChange(final Short val) {
            return new ObserveWaitStop(cm, val);
        }

        @Override
        public CaObserveSenderImpl.ObserveState onAbortMarkChange(final Short val) {
            return new ObserveWaitAbort(cm, val);
        }

    };

    // We are done, image is ready
    private final class ObserveDone implements ObserveState {
        final CarStateGeneric observeCarState;
        final CaCommandMonitorImpl cm;

        ObserveDone(final CarStateGeneric observeCarState, final CaCommandMonitorImpl cm) {
            this.observeCarState = observeCarState;
            this.cm = cm;
        }

        @Override
        public String signature() { return "ObserveDone(observeCarState = " + observeCarState + ")"; }

        @Override
        public ObserveState onObserveCarValChange(final CarStateGeneric carState) {
            return this;
        }

        @Override
        public CaObserveSenderImpl.ObserveState onStopMarkChange(final Short val) {
            return this;
        }

        @Override
        public CaObserveSenderImpl.ObserveState onAbortMarkChange(final Short val) {
            return this;
        }

        @Override
        public boolean isDone() { return true; }

    };

    // At this state we are paused and waiting for a resume
    private final class ObservePaused implements ObserveState {
        final CaCommandMonitorImpl cm;
        final CarStateGeneric observeCarState;

        ObservePaused(final CarStateGeneric observeCarState, final CaCommandMonitorImpl cm) {
            this.observeCarState = observeCarState;
            this.cm = cm;
        }

        public String signature() { return "ObservePaused(observeCarState = " + observeCarState + ")"; }

        public ObserveState onObserveCarValChange(final CarStateGeneric carState) {
            if (carState.isBusy()) {
                // Now we are busy, let's go to wait for idle
                return new ObserveWaitIdle(carState, cm);
            } else if (carState.isError()) {
                // Signal an error
                failCommandWithObserveCarError(cm);
                return idleObserveState;
            } else if (carState.isPaused()) {
                return this;
            } else {
                // No change, preserve the car
                return new ObservePaused(carState, cm);
            }
        }

        public CaObserveSenderImpl.ObserveState onStopMarkChange(final Short val) {
            return new ObserveWaitStop(cm, val);
        }

        public CaObserveSenderImpl.ObserveState onAbortMarkChange(final Short val) {
            return new ObserveWaitAbort(cm, val);
        }

        @Override
        public boolean isPaused() {
            return true;
        }

    }

    // Wait for stop to go idle
    private final class ObserveWaitStop implements ObserveState {
        final CaCommandMonitorImpl cm;
        final Short stopMark;

        ObserveWaitStop(final CaCommandMonitorImpl cm, final Short stopMark) {
            this.cm = cm;
            this.stopMark = stopMark;
        }

        @Override
        public String signature() { return "ObserveWaitStop(stopMark = " + stopMark + ")" ; }

        @Override
        public ObserveState onObserveCarValChange(final CarStateGeneric carState) {
            return this;
        }

        @Override
        public CaObserveSenderImpl.ObserveState onStopMarkChange(final Short val) {
            if (stopMark == MRK_PRESET && val == MRK_IDLE) {
                return new ObserveStop(cm);
            } else {
                return new ObserveWaitStop(cm, val);
            }
        }

        @Override
        public CaObserveSenderImpl.ObserveState onAbortMarkChange(final Short val) {
            return new ObserveWaitAbort(cm, val);
        }

    }

    // Wait for observe to be done
    private final class ObserveStop implements ObserveState {
        final CaCommandMonitorImpl cm;
        boolean isStopped = false;

        ObserveStop(final CaCommandMonitorImpl cm) {
            this.cm = cm;
        }

        @Override
        public String signature() { return "ObserveStop(isStopped = " + isStopped + ")"; }

        @Override
        public ObserveState onObserveCarValChange(final CarStateGeneric carState) {
            if (carState.isIdle()) {
                isStopped = true;
                return this;
            } else {
                return this;
            }
        }

        @Override
        public CaObserveSenderImpl.ObserveState onStopMarkChange(final Short val) {
            return this;
        }

        @Override
        public CaObserveSenderImpl.ObserveState onAbortMarkChange(final Short val) {
            return this;
        }

        @Override
        public boolean isStopped() { return isStopped; }
    }

    // Wait for abort to go idle
    private final class ObserveWaitAbort implements ObserveState {
        final CaCommandMonitorImpl cm;
        final Short abortMark;

        ObserveWaitAbort(final CaCommandMonitorImpl cm, final Short abortMark) {
            this.cm = cm;
            this.abortMark = abortMark;
        }

        @Override
        public String signature() { return "ObserveWaitAbort(abortMark = " + abortMark + ")" ; }

        @Override
        public ObserveState onObserveCarValChange(final CarStateGeneric carState) {
            return this;
        }

        @Override
        public CaObserveSenderImpl.ObserveState onStopMarkChange(final Short val) {
            return this;
        }

        @Override
        public CaObserveSenderImpl.ObserveState onAbortMarkChange(final Short val) {
          if (abortMark == MRK_PRESET && val == MRK_IDLE) {
              return new ObserveAbort(cm);
          } else {
              return new ObserveWaitAbort(cm, val);
          }
        }

    }

    // Wait for observe abourt to be done
    private final class ObserveAbort implements ObserveState {
        final CaCommandMonitorImpl cm;
        boolean isAborted = false;

        ObserveAbort(final CaCommandMonitorImpl cm) {
            this.cm = cm;
        }

        @Override
        public String signature() { return "ObserveAbort"; }

        @Override
        public ObserveState onObserveCarValChange(final CarStateGeneric carState) {
            if (carState.isIdle()) {
                isAborted = true;
                return this;
            } else {
                return this;
            }
        }

        @Override
        public CaObserveSenderImpl.ObserveState onStopMarkChange(final Short val) {
          return this;
        }

        @Override
        public CaObserveSenderImpl.ObserveState onAbortMarkChange(final Short val) {
          return this;
        }

        @Override
        public boolean isAborted() { return isAborted; }
    }

    // Initial state before any channel has changed
    protected static final class IdleState implements ApplyState {
        IdleState() { }

        @Override
        public String signature() { return "idleState"; }

        @Override
        public CaObserveSenderImpl.ApplyState onApplyValChange(final Integer val) {
            return this;
        }

        @Override
        public CaObserveSenderImpl.ApplyState onCarValChange(final CarStateGeneric carState) {
            return this;
        }

        @Override
        public CaObserveSenderImpl.ApplyState onCarClidChange(final Integer val) { return this; }

        @Override
        public CaObserveSenderImpl.ApplyState onTimeout() {
            return this;
        }

        @Override
        public CaObserveSenderImpl.ApplyState copyWithObserveState(final ObserveState observeState) {
            return this;
        }

        @Override
        public ObserveState currentObserveState() {
            return idleObserveState;
        }

        @Override
        public boolean isIdle() {
            return true;
        }


    };

    protected static IdleState idleState = new IdleState();

    // In this state we wait for clid to change
    private final class WaitApplyPreset implements CaObserveSenderImpl.ApplyState {
        final CaCommandMonitorImpl cm;
        final CarStateGeneric carState;
        final Integer carClid;
        final ObserveState observeState;

        WaitApplyPreset(final CaCommandMonitorImpl cm) {
            this.cm = cm;
            this.carState = null;
            this.carClid = null;
            this.observeState = new ObserveWaitBusy(cm);
        }

        private WaitApplyPreset(
            final CaCommandMonitorImpl cm,
            final CarStateGeneric carState,
            final Integer carClid,
            final ObserveState observeState) {
            this.cm = cm;
            this.carState = carState;
            this.carClid = carClid;
            this.observeState = observeState;
        }

        @Override
        public String signature() {
            return "WaitApplyPreset(carState = " + carState + ", carClid = " + carClid
                    + ", observeState = " + observeState.signature() + ")";
        }

        @Override
        public CaObserveSenderImpl.ApplyState onApplyValChange(final Integer val) {
            if (val > 0) {
                if (val.equals(carClid) && carState != null) {
                    if (carState.isError()) {
                        failCommandWithCarError(cm);
                        return idleState;
                    } else if (carState.isBusy()) {
                        return new CaObserveSenderImpl.WaitApplyIdle(cm, val, carState, observeState);
                    }
                }
                return new CaObserveSenderImpl.WaitApplyBusy(cm, val, carState, observeState);
            } else {
                failCommandWithApplyError(cm);
                return idleState;
            }
        }

        @Override
        public CaObserveSenderImpl.ApplyState onCarValChange(final CarStateGeneric val) {
            if ((carState == null || carState.isIdle()) && val.isBusy() && carClid != null && carClid > 0) {
                return new CaObserveSenderImpl.WaitApplyIdle(cm, carClid, val, observeState);
            } else {
                return new CaObserveSenderImpl.WaitApplyPreset(cm, val, carClid, observeState);
            }
        }

        @Override
        public CaObserveSenderImpl.ApplyState onCarClidChange(final Integer val) {
            return new CaObserveSenderImpl.WaitApplyPreset(cm, carState, val, observeState);
        }

        @Override
        public CaObserveSenderImpl.ApplyState onTimeout() {
            failCommand(cm, new TimeoutException());
            return idleState;
        }

        @Override
        public CaObserveSenderImpl.ApplyState copyWithObserveState(final ObserveState observeState) {
            return new CaObserveSenderImpl.WaitApplyPreset(cm, carState, carClid, observeState);
        }

        @Override
        public ObserveState currentObserveState() {
            return observeState;
        }

    }

    // In this stat we wait for applyC to turn to busy
    private final class WaitApplyBusy implements ApplyState {
        final CaCommandMonitorImpl cm;
        final int clid;
        final CarStateGeneric carState;
        final ObserveState observeState;

        WaitApplyBusy(
            final CaCommandMonitorImpl cm,
            final int clid,
            final CarStateGeneric carState,
            final ObserveState observeState) {
            this.cm = cm;
            this.clid = clid;
            this.carState = carState;
            this.observeState = observeState;
        }

        @Override
        public String signature() {
            return "WaitApplyBusy(clid = " + clid + ", observeState = " + observeState.signature() + ")";
        }

        @Override
        public ApplyState onApplyValChange(final Integer val) {
            if (val == clid) {
                return this;
            } else {
                failCommand(cm, new CaCommandPostError(
                        "Another command was triggered in apply record "
                                + apply.getEpicsName()));
                return idleState;
            }
        }

        @Override
        public ApplyState onCarValChange(final CarStateGeneric val) {
            if (val.isBusy()) {
                return new WaitApplyIdle(cm, clid, carState, observeState);
            }
            else if (val.isError()) {
                failCommandWithCarError(cm);
                return idleState;
            }
            else {
                if(val.isIdle() && observeState != idleObserveState) {
                    return (new CaObserveSenderImpl.WaitApplyIdle(cm, clid, val, observeState)).onCarValChange(val);
                }
                else {
                    return this;
                }
            }
        }

        @Override
        public ApplyState onCarClidChange(final Integer val) {
            if (val == clid) {
                return this;
            } else {
                failCommand(cm, new CaCommandPostError(
                        "Another command was triggered in apply record "
                                + apply.getEpicsName()));
                return idleState;
            }
        }

        @Override
        public ApplyState onTimeout() {
            failCommand(cm, new TimeoutException());
            return idleState;
        }

        @Override
        public CaObserveSenderImpl.ApplyState copyWithObserveState(final ObserveState observeState) {
            return new WaitApplyBusy(cm, clid, carState, observeState);
        }

        @Override
        public ObserveState currentObserveState() {
            return observeState;
        }
    }

    private final class WaitApplyIdle implements ApplyState {
        final CaCommandMonitorImpl cm;
        final int clid;
        final CarStateGeneric carState;
        final ObserveState observeState;

        WaitApplyIdle(
            final CaCommandMonitorImpl cm,
            final int clid,
            final CarStateGeneric carState,
            final ObserveState observeState) {
            this.cm = cm;
            this.clid = clid;
            this.carState = carState;
            this.observeState = observeState;
        }

        @Override
        public String signature() {
            return "WaitApplyIdle(clid = " + clid + ", observeState = " + observeState.signature() + ")";
        }

        @Override
        public ApplyState onApplyValChange(final Integer val) {
            if (val >= clid) {
                return new WaitApplyIdle(cm, val, carState, observeState);
            } else {
                failCommand(cm, new CaCommandPostError(
                        "Another command was triggered in apply record "
                                + apply.getEpicsName()));
                return idleState;
            }
        }

        @Override
        public ApplyState onCarValChange(final CarStateGeneric val) {
            if (val.isIdle()) {
                return checkOutCompletion(val, clid, observeState);
            }
            else if (val.isError()) {
                failCommandWithCarError(cm);
                return idleState;
            }
            else {
                return this;
            }
        }


        @Override
        public ApplyState onCarClidChange(final Integer val) {
            if (val >= clid) {
                return new WaitApplyIdle(cm, val, carState, observeState);
            }
            else {
                failCommand(cm, new CaCommandPostError(
                        "Another command was triggered in apply record "
                                + apply.getEpicsName()));
                return idleState;
            }
        }

        @Override
        public ApplyState onTimeout() {
            failCommand(cm, new TimeoutException());
            return idleState;
        }

        @Override
        public CaObserveSenderImpl.ApplyState onObserveCarValChange(final CarStateGeneric carState) {
            ObserveState state = currentObserveState().onObserveCarValChange(carState);
            if (state.isDone() || state.isAborted() || state.isStopped()) {
                return checkOutCompletion(carState, clid, state);
            } else if (carState.isError()) {
                return idleState;
            } else if (state.isPaused()) {
                return idleState;
            } else {
                return copyWithObserveState(state);
            }
        }

        private CaObserveSenderImpl.ApplyState checkOutCompletion(
            final CarStateGeneric carState,
            final Integer carClid,
            final ObserveState observeState) {
            if (carState != null && carClid != null && carClid == clid) {
                if (carState.isError()) {
                    failCommandWithCarError(cm);
                    return idleState;
                }
                if (observeState.isDone()) {
                    succedCommand(cm);
                    return idleState;
                }
                if (observeState.isStopped()) {
                    failCommand(cm, new CaObserveStopped());
                    return idleState;
                }
                if (observeState.isAborted()) {
                    failCommand(cm, new CaObserveAborted());
                    return idleState;
                }
            }
            return new WaitApplyIdle(cm, clid, carState, observeState);
        }

        @Override
        public CaObserveSenderImpl.ApplyState copyWithObserveState(final ObserveState observeState) {
            return new WaitApplyIdle(cm, clid, carState, observeState);
        }

        @Override
        public ObserveState currentObserveState() {
            return observeState;
        }
    }

    protected synchronized void onApplyValChange(final Integer val) {
        if (val != null) {
            ApplyState oldState = currentState;
            currentState = currentState.onApplyValChange(val);
            if (currentState.equals(idleState) && timeoutFuture != null) {
                timeoutFuture.cancel(true);
                timeoutFuture = null;
            }
            if (trace) LOG.debug("onApplyValChange(" + val + "): " + oldState.signature() + " -> " + currentState.signature());
        }
    }

    protected synchronized void onCarClidChange(final Integer val) {
        if (val != null) {
            ApplyState oldState = currentState;
            currentState = currentState.onCarClidChange(val);
            if (currentState.equals(idleState) && timeoutFuture != null) {
                timeoutFuture.cancel(true);
                timeoutFuture = null;
            }
            if (trace) LOG.debug("onCarClidChange(" + val + "): " + oldState.signature() + " -> " + currentState.signature());
        }
    }

    protected synchronized void onCarValChange(final C carState) {
        if (carState != null) {
            ApplyState oldState = currentState;
            currentState = currentState.onCarValChange(carState);
            if (currentState.equals(idleState) && timeoutFuture != null) {
                timeoutFuture.cancel(true);
                timeoutFuture = null;
            }
            if (trace) LOG.debug("onCarValChange(" + carState + "): " + oldState.signature() + " -> " + currentState.signature());
        }
    }

    protected synchronized void onObserveCarValChange(final C carState) {
        if (carState != null) {
            ApplyState oldState = currentState;
            currentState = currentState.onObserveCarValChange(carState);
            if (currentState.equals(idleState) && timeoutFuture != null) {
                timeoutFuture.cancel(true);
                timeoutFuture = null;
            }
            if (trace) LOG.debug("onObserveCarValChange(" + carState + "): " + oldState.signature() + " -> " + currentState.signature());
        }
    }

    protected synchronized void onStopMarkChange(final Short val) {
        if (val != null) {
            ApplyState oldState = currentState;
            currentState = currentState.onStopMarkChange(val);
            if (currentState.equals(idleState) && timeoutFuture != null) {
                timeoutFuture.cancel(true);
                timeoutFuture = null;
            }
            if (trace) LOG.debug("onStopMarkChange(" + val + "): " + oldState.signature() + " -> " + currentState.signature());
        }
    }

    protected synchronized void onAbortMarkChange(final Short val) {
        if (val != null) {
            ApplyState oldState = currentState;
            currentState = currentState.onAbortMarkChange(val);
            if (currentState.equals(idleState) && timeoutFuture != null) {
                timeoutFuture.cancel(true);
                timeoutFuture = null;
            }
            if (trace) LOG.debug("onAbortMarkChange(" + val + "): " + oldState.signature() + " -> " + currentState.signature());
        }
    }

    private synchronized void onTimeout() {
        timeoutFuture = null;
        ApplyState oldState = currentState;
        currentState = currentState.onTimeout();
        if (trace) LOG.debug("onTimeout: " + oldState.signature() + " -> " + currentState.signature());
    }

    protected ApplyState applyState() {
      return currentState;
    }

    @Override
    public synchronized boolean isActive() {
        return !currentState.equals(idleState);
    }

    @Override
    public synchronized void setTimeout(long timeout, TimeUnit timeUnit) {
        this.timeout = timeout;
        this.timeoutUnit = timeUnit;
    }

    private void succedCommand(final CaCommandMonitorImpl cm) {
        executor.execute(() -> cm.completeSuccess());
    }

    private void pauseCommand(final CaCommandMonitorImpl cm) {
        executor.execute(() -> cm.completePause());
    }

    private void failCommand(final CaCommandMonitorImpl cm, final Exception ex) {
        executor.execute(() -> cm.completeFailure(ex));
    }

    private void failCommandWithApplyError(final CaCommandMonitorImpl cm) {
        // I found that if I try to read OMSS or MESS from the same thread that
        // is processing a channel notifications, the reads fails with a
        // timeout. But it works if the read is done later from another thread.
        executor.execute(() -> {
            String msg = null;
            try {
                msg = apply.getMessValue();
            } catch (CAException | TimeoutException e) {
                LOG.warn(e.getMessage());
            }
            cm.completeFailure(new CaCommandError(msg));
        } );
    }

    private void failCommandWithCarError(final CaCommandMonitorImpl cm) {
        // I found that if I try to read OMSS or MESS from the same thread that
        // is processing a channel notifications, the reads fails with a
        // timeout. But it works if the read is done later from another thread.
        executor.execute(() -> {
            String msg = null;
            try {
                msg = car.getOmssValue();
            } catch (CAException | TimeoutException e) {
                LOG.warn(e.getMessage());
            }
            cm.completeFailure(new CaCommandError(msg));
        } );
    }

    private void failCommandWithObserveCarError(final CaCommandMonitorImpl cm) {
        // I found that if I try to read OMSS or MESS from the same thread that
        // is processing a channel notifications, the reads fails with a
        // timeout. But it works if the read is done later from another thread.
        executor.execute(() -> {
            String msg = null;
            try {
                msg = observeCar.getOmssValue();
            } catch (CAException | TimeoutException e) {
                LOG.warn(e.getMessage());
            }
            cm.completeFailure(new CaCommandError(msg));
        } );
    }

    @Override
    public String getDescription() {
        return description;
    }

    @Override
    public void clear() throws TimeoutException {
        try {
            apply.setDir(CadDirective.CLEAR);
        } catch (CAException e) {
            LOG.warn(e.getMessage());
        }
    }

}
