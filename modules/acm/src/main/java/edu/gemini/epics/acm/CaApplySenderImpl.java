/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Optional;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import edu.gemini.epics.EpicsReader;
import edu.gemini.epics.EpicsWriter;
import edu.gemini.epics.api.ChannelListener;
import gov.aps.jca.CAException;
import gov.aps.jca.TimeoutException;


/**
 * Monitors the execution of a command in an EPICS system using the Gemini EPICS records.
 * The command is triggered writing START to the DIR field of an apply record. The apply record then changes its output.
 * apply.VAL < 0 is an error. apply.VAL > 0 is an command id. The command is completed when the associated CAR record
 * changes from BUSY to IDLE with its field CLID > than the command id produced by the apply record.
 */
final class CaApplySenderImpl<C extends Enum<C> & CarStateGeneric> implements ApplySenderWithResource {

    private static final Logger LOG = LoggerFactory.getLogger(CaApplySenderImpl.class
            .getName());

    private final String name;
    private final String description;

    private final CaApplyRecord apply;
    private final CaCarRecord<C> car;

    private final Boolean trace = Boolean.getBoolean("epics.apply.trace");

    /*
     * Changes to apply.VAL, applyC.VAL and applyC.CLID can arrive in disorder. To fix that solution, a BUSY-IDLE
     * will be accepted even if it arrives before the applyC.VAL and/or applyC.CLID change, as long the delay is less
     * than `CompletionEventWindow`
     */
    static Duration CompletionEventWindow = Duration.ofMillis(50);

    private long timeout;
    private TimeUnit timeoutUnit;
    private final ScheduledExecutorService executor;
    private ScheduledFuture<?> timeoutFuture;
    private final ChannelListener<Integer> valListener;
    private final ChannelListener<Integer> carClidListener;
    private final ChannelListener<C> carValListener;
    private final TimestampProvider timestampProvider;
    private State currentState;
    private static final State IdleState = new State() {
        @Override
        public String signature() { return "IdleState"; }

        @Override
        public State onApplyValChange(Integer val, Instant timestamp) {
            return this;
        }

        @Override
        public State onCarValChange(CarStateGeneric carState, Instant timestamp) {
            return this;
        }

        @Override
        public State onCarClidChange(Integer val, Instant timestamp) {
            return this;
        }

        @Override
        public State onTimeout() {
            return this;
        }
    };

    private static final DateTimeFormatter timestampFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")
            .withZone(ZoneId.systemDefault());

    public CaApplySenderImpl(
            final String name,
            final String applyRecord,
            final String carRecord,
            final String description,
            final Class<C> carClass,
            final EpicsReader epicsReader,
            final EpicsWriter epicsWriter,
            final ScheduledExecutorService executor,
            TimestampProvider timestampProvider) throws CAException {
        super();
        this.name = name;
        this.description = description;
        this.timestampProvider = timestampProvider;
        this.currentState = IdleState;
        this.executor = executor;

        apply = new CaApplyRecord(applyRecord, epicsReader, epicsWriter);
        apply.registerValListener(valListener = (arg0, newVals) -> {
            if (newVals != null && !newVals.isEmpty()) {
                CaApplySenderImpl.this.onApplyValChange(newVals.get(0));
            }
        });

        car = new CaCarRecord<C>(carRecord, carClass, epicsReader);
        car.registerClidListener(carClidListener = (arg0, newVals) -> {
            if (newVals != null && !newVals.isEmpty()) {
                CaApplySenderImpl.this.onCarClidChange(newVals.get(0));
            }
        });
        car.registerValListener(carValListener = (arg0, newVals) -> {
            if (newVals != null && !newVals.isEmpty()) {
                CaApplySenderImpl.this.onCarValChange(newVals.get(0));
            }
        });

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

        apply.unbind();
        car.unbind();
    }

    @Override
    public synchronized CaCommandMonitor post() {
        CaCommandMonitorImpl cm = new CaCommandMonitorImpl();
        if (!currentState.equals(IdleState)) {
            failCommand(cm, new CaCommandInProgress());
        } else {
            try {
                currentState = new BusyState(cm);

                apply.setDir(CadDirective.START);
                if (timeout > 0) {
                    timeoutFuture = executor.schedule(CaApplySenderImpl.this::onTimeout, timeout, timeoutUnit);
                }
            } catch (CAException | TimeoutException e) {
                failCommand(cm, e);
                currentState = IdleState;
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

    private interface State {
        String signature();

        State onApplyValChange(Integer val, Instant timestamp);

        State onCarValChange(CarStateGeneric carState, Instant timestamp);

        State onCarClidChange(Integer val, Instant timestamp);

        State onTimeout();
    }

    /*
     * CommandState keeps track of the CAR output changes.
     */
    private interface CommandState {
        CommandState onCarValChange(CarStateGeneric carState, Instant timestamp);

        boolean checkCompletion(Instant eventTimestamp, int clid, int carClid, CaCommandMonitorImpl cm);

        String signature();
    }

    private final CommandState commandIdle = new CommandState() {
        @Override
        public CommandState onCarValChange(CarStateGeneric carState, Instant timestamp) {
            if(carState.isBusy()) return commandBusy;
            else if(carState.isIdle()) return this;
            else return new CommandEnded(timestamp, carState);
        }

        @Override
        public boolean checkCompletion(Instant eventTimestamp, int clid, int carClid, CaCommandMonitorImpl cm) {
            return false;
        }

        @Override
        public String signature() {
            return "CommandIdle";
        }
    };

    private final CommandState commandBusy = new CommandState() {
        @Override
        public CommandState onCarValChange(CarStateGeneric carState, Instant timestamp) {
            if(carState.isBusy()) return this;
            else return new CommandEnded(timestamp, carState);
        }

        @Override
        public boolean checkCompletion(Instant eventTimestamp, int clid, int carClid, CaCommandMonitorImpl cm) {
            return false;
        }

        @Override
        public String signature() {
            return "CommandBusy";
        }
    };

    private final class CommandEnded implements CommandState {
        private final Instant timestamp;
        private final CarStateGeneric carVal;

        CommandEnded(final Instant timestamp, final CarStateGeneric carVal) {
            this.timestamp = timestamp;
            this.carVal = carVal;
        }

        @Override
        public CommandState onCarValChange(CarStateGeneric carState, Instant timestamp) {
            if(carState.isBusy()) return commandBusy;
            else if(!carVal.isIdle() && carState.isIdle()) return commandIdle;
            else return new CommandEnded(timestamp, carVal);
        }

        @Override
        public boolean checkCompletion(Instant eventTimestamp, int clid, int carClid, CaCommandMonitorImpl cm) {
            if(carClid >= clid && timestamp.plus(CompletionEventWindow).isAfter(eventTimestamp)) {
                if (carVal.isPaused()) pauseCommand(cm);
                else if (carVal.isError()) failCommandWithCarError(cm);
                else succeedCommand(cm); //The only remaining possibility is IDLE, because it is not possible to reach this state on BUSY.

                return true;
            } else
                return false;
        }

        @Override
        public String signature() {
            return "CommandEnded(carVal = " + carVal + ", timestamp = " + timestampFormatter.format(timestamp) + ")";
        }
    }

    /*
     * BusyState keeps track of the clid given by the apply record, the clid in the CAR, and the sequence of changes in
     * the CAR output, and checks if the command is completed after every change.
     */
    private final class BusyState implements State {
        final CaCommandMonitorImpl cm;
        final CommandState commandState;
        final Optional<Integer> clid;
        final Optional<Integer> carClid;

        BusyState(final CaCommandMonitorImpl cm) {
            this.cm = cm;
            this.commandState = commandIdle;
            this.clid = Optional.empty();
            this.carClid = Optional.empty();
        }

        BusyState(final CaCommandMonitorImpl cm,
                  final CommandState commandState,
                  final Optional<Integer> clid,
                  final Optional<Integer> carClid) {
            this.cm = cm;
            this.commandState = commandState;
            this.clid = clid;
            this.carClid = carClid;
        }

        @Override
        public String signature() {
            return "BusyState(commandState = " + commandState.signature() + ", clid = " + clid + ", carClid = " + carClid + ")";
        }

        @Override
        public State onApplyValChange(Integer val, Instant timestamp) {
            if(val > 0) {
                if(clid.isPresent()) return this;
                else {
                    boolean ended = carClid.map(y -> commandState.checkCompletion(timestamp, val, y, cm)).orElse(false);
                    if (ended) return IdleState;
                    else return new BusyState(cm, commandState, Optional.of(val), carClid);
                }
            } else {
                failCommandWithApplyError(cm);
                return IdleState;
            }
        }

        @Override
        public State onCarValChange(CarStateGeneric carState, Instant timestamp) {
            CommandState newCmdState = commandState.onCarValChange(carState, timestamp);
            boolean ended = clid.flatMap( x -> carClid.map(y -> newCmdState.checkCompletion(timestamp, x, y, cm))).orElse(false);
            if(ended) return IdleState;
            else return new BusyState(cm, newCmdState, clid, carClid);
        }

        @Override
        public State onCarClidChange(Integer val, Instant timestamp) {
            boolean ended = clid.map(x -> commandState.checkCompletion(timestamp, x, val, cm)).orElse(false);
            if (ended) return IdleState;
            else return new BusyState(cm, commandState, clid, Optional.of(val));
        }

        @Override
        public State onTimeout() {
            failCommand(cm, new TimeoutException());
            return IdleState;
        }
    }

    protected synchronized void onApplyValChange(final Integer val, Instant timestamp) {
        if (val != null) {
            State oldState = currentState;
            currentState = currentState.onApplyValChange(val, timestamp);
            if (currentState.equals(IdleState) && timeoutFuture != null) {
                timeoutFuture.cancel(true);
                timeoutFuture = null;
            }
            if(trace) LOG.debug("onApplyValChange(" + val + ", " + timestampFormatter.format(timestamp) + "): " + oldState.signature() + " -> " + currentState.signature());
        }
    }

    private synchronized void onApplyValChange(final Integer val) {
        onApplyValChange(val, timestampProvider.now());
    }

    protected synchronized void onCarClidChange(final Integer val, Instant timestamp) {
        if (val != null) {
            State oldState = currentState;
            currentState = currentState.onCarClidChange(val, timestamp);
            if (currentState.equals(IdleState) && timeoutFuture != null) {
                timeoutFuture.cancel(true);
                timeoutFuture = null;
            }
            if(trace) LOG.debug("onCarClidChange(" + val + ", " + timestampFormatter.format(timestamp) + "): " + oldState.signature() + " -> " + currentState.signature());
        }
    }

    private synchronized void onCarClidChange(final Integer val) {
        onCarClidChange(val, timestampProvider.now());
    }

    protected synchronized void onCarValChange(final C carState, Instant timestamp) {
        if (carState != null) {
            State oldState = currentState;
            currentState = currentState.onCarValChange(carState, timestamp);
            if (currentState.equals(IdleState) && timeoutFuture != null) {
                timeoutFuture.cancel(true);
                timeoutFuture = null;
            }
            if(trace) LOG.debug("onCarValChange(" + carState + ", " + timestampFormatter.format(timestamp) + "): " + oldState.signature() + " -> " + currentState.signature());
        }
    }

    private synchronized void onCarValChange(final C carState) {
        onCarValChange(carState, timestampProvider.now());
    }

    private synchronized void onTimeout() {
        timeoutFuture = null;
        State oldState = currentState;
        currentState = currentState.onTimeout();
        if(trace) LOG.debug("onTimeout: " + oldState.signature() + " -> " + currentState.signature());
    }

    @Override
    public synchronized boolean isActive() {
        return !currentState.equals(IdleState);
    }

    @Override
    public synchronized void setTimeout(final long timeout, final TimeUnit timeUnit) {
        this.timeout = timeout;
        this.timeoutUnit = timeUnit;
    }

    private void succeedCommand(final CaCommandMonitorImpl cm) {
        executor.execute(cm::completeSuccess);
    }

    private void pauseCommand(final CaCommandMonitorImpl cm) {
        executor.execute(cm::completePause);
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
        });
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
        });
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
