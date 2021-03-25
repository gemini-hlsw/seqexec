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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ThreadFactory;

/*
 * Class to handle observe commands in instruments were there is no CAR for the apply. In those cases, The observe CAR
 * handles all the command state.
 */
public class CaSimpleObserveSenderImpl<C extends Enum<C> & CarStateGeneric> implements ApplySenderWithResource {
    private static final Logger LOG = LoggerFactory.getLogger(CaSimpleObserveSenderImpl.class.getName());
    private static final String CAD_MARK_SUFFIX = ".MARK";

    private final String name;
    private final String description;

    private final CaApplyRecord apply;
    private final CaCarRecord<C> car;
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
    private ChannelListener<Short> abortMarkListener;
    private ChannelListener<Short> stopMarkListener;
    private CaSimpleObserveSenderImpl.ApplyState currentState;

    CaSimpleObserveSenderImpl(
            final String name,
            final String applyRecord,
            final String carRecord,
            final String stopCmd,
            final String abortCmd,
            final String description,
            final Class<C> carClass,
            final EpicsReader epicsReader,
            final EpicsWriter epicsWriter,
            ScheduledExecutorService executor
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
                CaSimpleObserveSenderImpl.this.onApplyValChange(newVals.get(0));
            }
        });

        car = new CaCarRecord<C>(carRecord, carClass, epicsReader);
        // applyC.CLID int > 0
        car.registerClidListener(carClidListener = (String arg0, List<Integer> newVals) -> {
            if (newVals != null && !newVals.isEmpty()) {
                CaSimpleObserveSenderImpl.this.onCarClidChange(newVals.get(0));
            }
        });
        // applyC.VAL BUSY/IDLE
        car.registerValListener(carValListener = (String arg0, List<C> newVals) -> {
            if (newVals != null && !newVals.isEmpty()) {
                CaSimpleObserveSenderImpl.this.onCarValChange(newVals.get(0));
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
                        CaSimpleObserveSenderImpl.this.onStopMarkChange(newVals.get(0));
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
                        CaSimpleObserveSenderImpl.this.onAbortMarkChange(newVals.get(0));
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
                currentState = new CaSimpleObserveSenderImpl.WaitApplyPreset(cm);
                if (timeout > 0) {
                    timeoutFuture = executor.schedule(CaSimpleObserveSenderImpl.this::onTimeout, timeout, timeoutUnit);
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

        CaSimpleObserveSenderImpl.ApplyState onApplyValChange(final Integer val);

        CaSimpleObserveSenderImpl.ApplyState onCarValChange(final CarStateGeneric carState);

        CaSimpleObserveSenderImpl.ApplyState onCarClidChange(final Integer val);

        default CaSimpleObserveSenderImpl.ApplyState onStopMarkChange(final Short val) {
            return this;
        }

        default CaSimpleObserveSenderImpl.ApplyState onAbortMarkChange(final Short val) {
            return this;
        }

        default boolean isIdle() {
            return false;
        }

        CaSimpleObserveSenderImpl.ApplyState onTimeout();
    }


    // Initial state before any channel has changed
    protected static final class IdleState implements ApplyState {
        IdleState() { }

        @Override
        public String signature() { return "idleState"; }

        @Override
        public CaSimpleObserveSenderImpl.ApplyState onApplyValChange(final Integer val) {
            return this;
        }

        @Override
        public CaSimpleObserveSenderImpl.ApplyState onCarValChange(final CarStateGeneric carState) {
            return this;
        }

        @Override
        public CaSimpleObserveSenderImpl.ApplyState onCarClidChange(final Integer val) { return this; }

        @Override
        public CaSimpleObserveSenderImpl.ApplyState onTimeout() {
            return this;
        }

        @Override
        public boolean isIdle() {
            return true;
        }


    }

    private static final IdleState idleState = new IdleState();

    // In this state we wait for clid to change
    private final class WaitApplyPreset implements CaSimpleObserveSenderImpl.ApplyState {
        final CaCommandMonitorImpl cm;
        final CarStateGeneric carState;
        final Integer carClid;

        WaitApplyPreset(final CaCommandMonitorImpl cm) {
            this.cm = cm;
            this.carState = null;
            this.carClid = null;
        }

        private WaitApplyPreset(
                final CaCommandMonitorImpl cm,
                final CarStateGeneric carState,
                final Integer carClid) {
            this.cm = cm;
            this.carState = carState;
            this.carClid = carClid;
        }

        @Override
        public String signature() {
            return "WaitApplyPreset(carState = " + carState + ", carClid = " + carClid + ")";
        }

        @Override
        public CaSimpleObserveSenderImpl.ApplyState onApplyValChange(final Integer val) {
            if (val > 0) {
                if (val.equals(carClid) && carState != null) {
                    if (carState.isError()) {
                        failCommandWithCarError(cm);
                        return idleState;
                    } else if (carState.isBusy()) {
                        return new CaSimpleObserveSenderImpl.WaitApplyBusy(cm, val, carState);
                    }
                }
                return new CaSimpleObserveSenderImpl.WaitApplyBusy(cm, val, carState);
            } else {
                failCommandWithApplyError(cm);
                return idleState;
            }
        }

        @Override
        public CaSimpleObserveSenderImpl.ApplyState onCarValChange(final CarStateGeneric val) {
            if ((carState == null || carState.isIdle()) && val.isBusy() && carClid != null && carClid > 0) {
                return new CaSimpleObserveSenderImpl.WaitObserveCompleted(cm, carClid);
            } else {
                return new CaSimpleObserveSenderImpl.WaitApplyPreset(cm, val, carClid);
            }
        }

        @Override
        public CaSimpleObserveSenderImpl.ApplyState onCarClidChange(final Integer val) {
            return new CaSimpleObserveSenderImpl.WaitApplyPreset(cm, carState, val);
        }

        @Override
        public CaSimpleObserveSenderImpl.ApplyState onTimeout() {
            failCommand(cm, new TimeoutException());
            return idleState;
        }

    }

    // In this stat we wait for applyC to turn to busy
    private final class WaitApplyBusy implements ApplyState {
        final CaCommandMonitorImpl cm;
        final int clid;
        final CarStateGeneric carState;

        WaitApplyBusy(
                final CaCommandMonitorImpl cm,
                final int clid,
                final CarStateGeneric carState) {
            this.cm = cm;
            this.clid = clid;
            this.carState = carState;
        }

        @Override
        public String signature() {
            return "WaitApplyBusy(clid = " + clid + ")";
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
                return new WaitObserveCompleted(cm, clid);
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

    }

    private final class WaitObserveCompleted implements ApplyState {
        final CaCommandMonitorImpl cm;
        final int clid;
        final short stopMark;
        final short abortMark;

        WaitObserveCompleted(
                final CaCommandMonitorImpl cm,
                final int clid) {
            this.cm = cm;
            this.clid = clid;
            this.stopMark = MRK_IDLE;
            this.abortMark = MRK_IDLE;
        }

        WaitObserveCompleted(
                final CaCommandMonitorImpl cm,
                final int clid,
                short stop,
                short abort) {
            this.cm = cm;
            this.clid = clid;
            this.stopMark = stop;
            this.abortMark = abort;
        }

        @Override
        public String signature() {
            return "WaitObserveCompleted(clid = " + clid + ")";
        }

        @Override
        public ApplyState onApplyValChange(final Integer val) {
            if (val >= clid) {
                return new WaitObserveCompleted(cm, val, stopMark, abortMark);
            } else {
                failCommand(cm, new CaCommandPostError(
                        "Another command was triggered in apply record " + apply.getEpicsName()
                ));
                return idleState;
            }
        }

        @Override
        public ApplyState onCarValChange(final CarStateGeneric val) {
            if (val != null) {
                if (val.isError()) {
                    failCommandWithCarError(cm);
                    return idleState;
                }
                if (val.isIdle()) {
                    succedCommand(cm);
                    return idleState;
                }
                if (val.isPaused()) {
                    pauseCommand(cm);
                    return idleState;
                }
            }
            return this;
        }


        @Override
        public ApplyState onCarClidChange(final Integer val) {
            if (val >= clid) {
                return new WaitObserveCompleted(cm, val);
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
        public ApplyState onStopMarkChange(Short val) {
            if(stopMark == MRK_PRESET && val == MRK_IDLE) {
                return new WaitStopCompleted(cm, clid);
            } else {
                return new WaitObserveCompleted(cm, clid, val, abortMark);
            }
        }

        @Override
        public ApplyState onAbortMarkChange(Short val) {
            if(abortMark == MRK_PRESET && val == MRK_IDLE) {
                return new WaitAbortCompletion(cm);
            } else {
                return new WaitObserveCompleted(cm, clid, stopMark, val);
            }
        }
    }
    private final class WaitStopCompleted implements ApplyState {
        final CaCommandMonitorImpl cm;
        final int clid;

        WaitStopCompleted(CaCommandMonitorImpl cm, int clid) {
            this.cm = cm;
            this.clid = clid;
        }

        @Override
        public String signature() {
            return "WaitStopCompleted( clid = " + clid + ", stopMark = " + stopMark + ", abortMark" + abortMark + ")";
        }

        @Override
        public ApplyState onApplyValChange(final Integer val) {
            if (val >= clid) {
                return new WaitStopCompleted(cm, val);
            } else {
                failCommand(cm, new CaCommandPostError(
                        "Another command was triggered in apply record " + apply.getEpicsName()
                ));
                return idleState;
            }
        }

        @Override
        public ApplyState onCarClidChange(final Integer val) {
            if (val >= clid) {
                return new WaitStopCompleted(cm, val);
            }
            else {
                failCommand(cm, new CaCommandPostError(
                        "Another command was triggered in apply record "
                                + apply.getEpicsName()));
                return idleState;
            }
        }

        @Override
        public ApplyState onCarValChange(CarStateGeneric val) {
            if(val.isIdle()) {
                failCommand(cm, new CaObserveStopped());
                return idleState;
            }
            else if(val.isError()) {
                failCommandWithCarError(cm);
                return idleState;
            }
            else if(val.isPaused()) {
                pauseCommand(cm);
                return idleState;
            }
            else {
                return this;
            }
        }

        @Override
        public ApplyState onTimeout() {
            failCommand(cm, new TimeoutException());
            return idleState;
        }

        @Override
        public ApplyState onStopMarkChange(Short val) {
            return this;
        }

        @Override
        public ApplyState onAbortMarkChange(Short val) {
            return this;
        }

    }

    private final class WaitAbortCompletion implements ApplyState {
        final CaCommandMonitorImpl cm;

        WaitAbortCompletion(CaCommandMonitorImpl cm) {
            this.cm = cm;
        }

        @Override
        public String signature() {
            return "WaitAbortCompletion( stopMark = " + stopMark + ", abortMark = " + abortMark + ")";
        }

        @Override
        public ApplyState onApplyValChange(Integer val) {
            return this;
        }

        @Override
        public ApplyState onCarValChange(CarStateGeneric val)  {
            if(val.isIdle()) {
                failCommand(cm, new CaObserveAborted());
                return idleState;
            }
            else if(val.isError()) {
                failCommandWithCarError(cm);
                return idleState;
            }
            else if(val.isPaused()) {
                pauseCommand(cm);
                return idleState;
            }
            else {
                return this;
            }
        }

        @Override
        public ApplyState onCarClidChange(Integer val) { return this; }

        @Override
        public ApplyState onTimeout() {
            failCommand(cm, new TimeoutException());
            return idleState;
        }

        @Override
        public ApplyState onStopMarkChange(Short val) {
            return this;
        }

        @Override
        public ApplyState onAbortMarkChange(Short val) {
            return this;
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

    protected synchronized void onTimeout() {
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
