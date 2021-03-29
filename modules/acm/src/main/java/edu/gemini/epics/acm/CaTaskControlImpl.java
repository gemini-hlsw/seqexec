/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import edu.gemini.epics.EpicsService;
import edu.gemini.epics.EpicsWriter;
import edu.gemini.epics.api.ChannelListener;
import edu.gemini.epics.impl.EpicsWriterImpl;
import gov.aps.jca.CAException;
import gov.aps.jca.TimeoutException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Set;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

interface TaskControlWithResource extends CaTaskControl, CaResource {}

public class CaTaskControlImpl implements TaskControlWithResource {
    private static final Logger LOG = LoggerFactory.getLogger(CaTaskControlImpl.class.getName());

    private final String name;
    private final String description;

    private final Boolean trace = true;

    private long timeout;
    private TimeUnit timeoutUnit;
    private final ScheduledExecutorService executor;
    private ScheduledFuture<?> timeoutFuture;
    private final ChannelListener<Integer> valListener;
    private final ChannelListener<TaskControlState> stateListener;
    private final CaParameterStorage storage;
    private final CaTaskControlRecord record;

    private State currentState = IdleState;
    private static final State IdleState = new State() {
        @Override
        public String signature() {
            return "IdleState";
        }

        @Override
        public State onValChange(Integer val) {
            return this;
        }

        @Override
        public State onStateChange(TaskControlState st) {
            return this;
        }

        @Override
        public State onTimeout() {
            return this;
        }
    };

    public CaTaskControlImpl(String name, String channel, String description, EpicsService epicsService,
                             ScheduledExecutorService executor)
            throws CAException {
        this.name = name;
        this.description = description;
        this.executor = executor;

        EpicsWriter epicsWriter = new EpicsWriterImpl(epicsService);
        storage = new CaParameterStorage(epicsWriter);
        record = new CaTaskControlRecord(channel, epicsService);

        record.registerValListener(valListener = new ChannelListener<Integer>() {
            @Override
            public void valueChanged(String arg0, List<Integer> newVals) {
                if (newVals != null && !newVals.isEmpty()) {
                    CaTaskControlImpl.this.onValChange(newVals.get(0));
                }
            }
        });

        record.registerStateListener(stateListener = new ChannelListener<TaskControlState>() {
            @Override
            public void valueChanged(String arg0, List<TaskControlState> newVals) {
                if (newVals != null && !newVals.isEmpty()) {
                    CaTaskControlImpl.this.onStateChange(newVals.get(0));
                }
            }
        });

    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getApply() {
        return null;
    }

    @Override
    public String getCAR() {
        return null;
    }

    @Override
    public String getDescription() {
        return description;
    }

    @Override
    public Set<String> getInfo() {
        return storage.getInfo();
    }

    @Override
    public CaApplySender getApplySender() {
        return this;
    }

    @Override
    public void mark() throws TimeoutException {
        try {
            record.setDir(CadDirective.MARK);
        } catch (CAException e) {
            LOG.warn(e.getMessage());
        }
    }

    @Override
    public void setTimeout(long timeout, TimeUnit timeUnit) {
        this.timeout = timeout;
        this.timeoutUnit = timeUnit;
    }

    @Override
    public boolean isActive() {
            return !currentState.equals(IdleState);
        }

    @Override
    public void clear() throws TimeoutException {
        try {
            record.setDir(CadDirective.CLEAR);
        } catch (CAException e) {
            LOG.warn(e.getMessage());
        }
    }

    @Override
    public CaParameter<Integer> addInteger(String name, String channel, String description, boolean isCADParameter)
            throws CaException {
        return storage.addInteger(name, channel, description, isCADParameter);
    }

    @Override
    public CaParameter<Double> addDouble(String name, String channel, String description, boolean isCADParameter)
            throws CaException {
        return storage.addDouble(name, channel, description, isCADParameter);
    }

    @Override
    public CaParameter<Float> addFloat(String name, String channel, String description, boolean isCADParameter)
            throws CaException {
        return storage.addFloat(name, channel, description, isCADParameter);
    }

    @Override
    public <T extends Enum<T>> CaParameter<T> addEnum(String name, String channel, Class<T> enumType,
                                                      String description, boolean isCADParameter) throws CaException {
        return storage.addEnum(name, channel, enumType, description, isCADParameter);
    }

    @Override
    public CaParameter<String> addString(String name, String channel, String description, boolean isCADParameter)
            throws CaException {
        return storage.addString(name, channel, description, isCADParameter);
    }

    @Override
    public void remove(String name) {
        storage.remove(name);
    }

    @Override
    public CaParameter<Integer> getInteger(String name) {
        return storage.getInteger(name);
    }

    @Override
    public CaParameter<Double> getDouble(String name) {
        return storage.getDouble(name);
    }

    @Override
    public CaParameter<Float> getFloat(String name) {
        return storage.getFloat(name);
    }

    @Override
    public CaParameter<String> getString(String name) {
        return storage.getString(name);
    }

    @Override
    public synchronized CaCommandMonitor post() {
        CaCommandMonitorImpl cm = new CaCommandMonitorImpl();
        if (!currentState.equals(IdleState)) {
            failCommand(cm, new CaCommandInProgress());
        } else {
            try {
                currentState = new WaitPreset(cm);

                record.setDir(CadDirective.START);
                if (timeout > 0) {
                    timeoutFuture = executor.schedule(() -> CaTaskControlImpl.this.onTimeout(), timeout, timeoutUnit);
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

    @Override
    public void unbind() {

        executor.shutdown();

        try {
            record.unregisterValListener(valListener);
        } catch (CAException e) {
            LOG.warn(e.getMessage());
        }
        try {
            record.unregisterStateListener(stateListener);
        } catch (CAException e) {
            LOG.warn(e.getMessage());
        }

        storage.unbind();
    }

    private interface State {
        String signature();
        State onValChange(Integer val);
        State onStateChange(TaskControlState st);
        State onTimeout();
    }

    private final class WaitPreset implements State {
        private final CaCommandMonitorImpl cm;
        private TaskControlState recState;

        private WaitPreset(final CaCommandMonitorImpl cm, TaskControlState recState) {
            this.cm = cm;
            this.recState = recState;
        }

        WaitPreset(final CaCommandMonitorImpl cm) {
            this.cm = cm;
            this.recState = null;
        }

        @Override
        public String signature() {
            return "WaitPreset(recState = " + recState + ")";
        }

        @Override
        public State onValChange(Integer val) {
            if(val == 0) {
                // BUSY output shouldn't change to BUSY before VAL changes to 0, but let me be paranoid
                if(recState != null && recState == TaskControlState.BUSY) {
                    return new WaitCompletion(cm);
                } else {
                    return new WaitStart(cm);
                }
            } else {
                failCommandWithErrorMessage(cm);
                return IdleState;
            }
        }

        @Override
        public State onStateChange(TaskControlState st) {
            if(st == TaskControlState.ERR) {
                failCommandWithErrorMessage(cm);
                return IdleState;
            } else {
                return new WaitPreset(cm, st);
            }
        }

        @Override
        public State onTimeout() {
            failCommand(cm, new TimeoutException());
            return IdleState;
        }
    }

    private final class WaitStart implements State {

        private final CaCommandMonitorImpl cm;

        WaitStart(CaCommandMonitorImpl cm) {
            this.cm = cm;
        }

        @Override
        public String signature() {
            return "WaitStart";
        }

        @Override
        public State onValChange(Integer val) {
            if(val == 0) {
                return this;
            } else {
                failCommandWithErrorMessage(cm);
                return IdleState;
            }
        }

        @Override
        public State onStateChange(TaskControlState st) {
            if(st == TaskControlState.ERR) {
                failCommandWithErrorMessage(cm);
                return IdleState;
            } else if(st == TaskControlState.BUSY) {
                return new WaitCompletion(cm);
            } else {
                return this;
            }
        }

        @Override
        public State onTimeout() {
            failCommand(cm, new TimeoutException());
            return IdleState;
        }
    }

    private final class WaitCompletion implements State {
        private final CaCommandMonitorImpl cm;

        WaitCompletion(CaCommandMonitorImpl cm) {
            this.cm = cm;
        }

        @Override
        public String signature() {
            return "WaitCompletion";
        }

        @Override
        public State onValChange(Integer val) {
            if(val == 0) {
                return this;
            } else {
                failCommandWithErrorMessage(cm);
                return IdleState;
            }
        }

        @Override
        public State onStateChange(TaskControlState st) {
            if(st == TaskControlState.ERR) {
                failCommandWithErrorMessage(cm);
                return IdleState;
            } else if(st == TaskControlState.IDLE) {
                succeedCommand(cm);
                return IdleState;
            } else {
                return this;
            }
        }

        @Override
        public State onTimeout() {
            failCommand(cm, new TimeoutException());
            return IdleState;
        }
    }

    private synchronized void onValChange(final Integer val) {
        if (val != null) {
            State oldState = currentState;
            currentState = currentState.onValChange(val);
            if (currentState.equals(IdleState) && timeoutFuture != null) {
                timeoutFuture.cancel(true);
                timeoutFuture = null;
            }
            if(trace) LOG.debug("onValChange(" + val + "): " + oldState.signature() + " -> " + currentState.signature());
        }
    }

    private synchronized void onStateChange(final TaskControlState val) {
        if (val != null) {
            State oldState = currentState;
            currentState = currentState.onStateChange(val);
            if (currentState.equals(IdleState) && timeoutFuture != null) {
                timeoutFuture.cancel(true);
                timeoutFuture = null;
            }
            if(trace) LOG.debug("onStateChange(" + val + "): " + oldState.signature() + " -> " + currentState.signature());
        }
    }

    private synchronized void onTimeout() {
        timeoutFuture = null;
        State oldState = currentState;
        currentState = currentState.onTimeout();
        if(trace) LOG.debug("onTimeout: " + oldState.signature() + " -> " + currentState.signature());
    }

    private void succeedCommand(final CaCommandMonitorImpl cm) {
        executor.execute(() -> cm.completeSuccess());
    }

    private void failCommand(final CaCommandMonitorImpl cm, final Exception ex) {
        executor.execute(() -> cm.completeFailure(ex));
    }

    private void failCommandWithErrorMessage(final CaCommandMonitorImpl cm) {
        executor.execute(() -> {
                    String msg = null;
                    try {
                        msg = record.getMessValue();
                    } catch (CAException | TimeoutException e) {
                        LOG.warn(e.getMessage());
                    }
                    cm.completeFailure(new CaCommandError(msg));
                });
    }


}
