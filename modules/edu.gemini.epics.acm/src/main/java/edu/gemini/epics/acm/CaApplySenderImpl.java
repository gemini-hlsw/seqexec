/*
 * Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import edu.gemini.epics.EpicsService;
import edu.gemini.epics.api.ChannelListener;
import gov.aps.jca.CAException;
import gov.aps.jca.TimeoutException;

final class CaApplySenderImpl implements CaApplySender {

    private static final Logger LOG = Logger.getLogger(CaApplySenderImpl.class
            .getName());

    private final String name;
    private final String description;
    
    private final CaApplyRecord apply;
    private final CaCarRecord car;
    
    private long timeout;
    private TimeUnit timeoutUnit;
    private ScheduledExecutorService executor;
    private ScheduledFuture<?> timeoutFuture;
    private final ChannelListener<Integer> valListener;
    private ChannelListener<Integer> carClidListener;
    private ChannelListener<CarState> carValListener;
    private State currentState;
    private static final State IdleState = new State() {
        @Override
        public State onApplyValChange(Integer val) {
            return this;
        }

        @Override
        public State onCarValChange(CarState carState) {
            return this;
        }

        @Override
        public State onCarClidChange(Integer val) {
            return this;
        }

        @Override
        public State onTimeout() {
            return this;
        }
    };

    public CaApplySenderImpl(String name, String applyRecord, String carRecord,
            String description, EpicsService epicsService) throws CAException {
        super();
        this.name = name;
        this.description = description;
        this.currentState = IdleState;

        apply = new CaApplyRecord(applyRecord, epicsService);
        apply.registerValListener(valListener = new ChannelListener<Integer>() {
            @Override
            public void valueChanged(String arg0, List<Integer> newVals) {
                if (newVals != null && !newVals.isEmpty()) {
                    CaApplySenderImpl.this.onApplyValChange(newVals.get(0));
                }
            }
        });
        
        car = new CaCarRecord(carRecord, epicsService);
        car.registerClidListener(carClidListener = new ChannelListener<Integer>() {
            @Override
            public void valueChanged(String arg0, List<Integer> newVals) {
                if (newVals != null && !newVals.isEmpty()) {
                    CaApplySenderImpl.this.onCarClidChange(newVals.get(0));
                }
            }
        });
        car.registerValListener(carValListener = new ChannelListener<CarState>() {
            @Override
            public void valueChanged(String arg0, List<CarState> newVals) {
                if (newVals != null && !newVals.isEmpty()) {
                    CaApplySenderImpl.this.onCarValChange(newVals.get(0));
                }
            }
        });

        executor = new ScheduledThreadPoolExecutor(2);
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

    void unbind() {
        
        executor.shutdown();

        try {
            apply.unregisterValListener(valListener);
        } catch (CAException e) {
            LOG.warning(e.getMessage());
        }
        try {
            car.unregisterClidListener(carClidListener);
        } catch (CAException e) {
            LOG.warning(e.getMessage());
        }
        try {
            car.unregisterValListener(carValListener);
        } catch (CAException e) {
            LOG.warning(e.getMessage());
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
            currentState = new WaitPreset(cm);

            try {
                apply.setDir(CadDirective.START);
            } catch (CAException | TimeoutException e) {
                failCommand(cm, e);
            }

            if (timeout > 0) {
                timeoutFuture = executor.schedule(new Runnable() {
                    @Override
                    public void run() {
                        CaApplySenderImpl.this.onTimeout();
                    }
                }, timeout, timeoutUnit);
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
    public CaCommandMonitor postCallback(CaCommandListener callback) {
        CaCommandMonitor cm = post();
        cm.setCallback(callback);
        return cm;
    }

    private interface State {
        State onApplyValChange(Integer val);

        State onCarValChange(CarState carState);

        State onCarClidChange(Integer val);

        State onTimeout();
    }

    private final class WaitPreset implements State {
        final CaCommandMonitorImpl cm;
        final CarState carVal;
        final Integer carClid;

        WaitPreset(CaCommandMonitorImpl cm) {
            this.cm = cm;
            this.carVal = null;
            this.carClid = null;
        }

        private WaitPreset(CaCommandMonitorImpl cm, CarState carVal, Integer carClid) {
            this.cm = cm;
            this.carVal = carVal;
            this.carClid = carClid;
        }

        @Override
        public State onApplyValChange(Integer val) {
            if (val > 0) {
                if (carClid != null && carClid.equals(val)) {
                    if (carVal == CarState.ERROR) {
                        failCommandWithCarError(cm);
                        return IdleState;
                    } else if (carVal == CarState.BUSY) {
                        return new WaitCompletion(cm, val);
                    }
                }
                return new WaitStart(cm, val, carVal, carClid);
            } else {
                failCommandWithApplyError(cm);
                return IdleState;
            }
        }

        @Override
        public State onCarValChange(CarState val) {
            return new WaitPreset(cm, val, carClid);
        }

        @Override
        public State onCarClidChange(Integer val) {
            return new WaitPreset(cm, carVal, val);
        }

        @Override
        public State onTimeout() {
            failCommand(cm, new TimeoutException());
            return IdleState;
        }
    }

    private final class WaitStart implements State {
        final CaCommandMonitorImpl cm;
        final int clid;
        final Integer carClid;
        final CarState carState;

        WaitStart(CaCommandMonitorImpl cm, int clid, CarState carState,
                Integer carClid) {
            this.cm = cm;
            this.clid = clid;
            this.carState = carState;
            this.carClid = carClid;
        }

        @Override
        public State onApplyValChange(Integer val) {
            if (val == clid) {
                return this;
            } else {
                failCommand(cm, new CaCommandPostError(
                        "Another command was triggered in apply record "
                                + apply.getEpicsName()));
                return IdleState;
            }
        }

        @Override
        public State onCarValChange(CarState val) {
            return checkOutConditions(val, carClid);
        }

        @Override
        public State onCarClidChange(Integer val) {
            return checkOutConditions(carState, val);
        }

        @Override
        public State onTimeout() {
            failCommand(cm, new TimeoutException());
            return IdleState;
        }

        private State checkOutConditions(CarState carState,
                Integer carClid) {
            if (carClid != null && carClid == clid) {
                if (carState == CarState.ERROR) {
                    failCommandWithCarError(cm);
                    return IdleState;
                }
                if (carState == CarState.BUSY) {
                    return new WaitCompletion(cm, clid);
                }
            }
            return new WaitStart(cm, clid, carState, carClid);
        }

    }

    private final class WaitCompletion implements State {
        final CaCommandMonitorImpl cm;
        final int clid;

        WaitCompletion(CaCommandMonitorImpl cm, int clid) {
            this.cm = cm;
            this.clid = clid;
        }

        @Override
        public State onApplyValChange(Integer val) {
            if (val == clid) {
                return this;
            } else {
                failCommand(cm, new CaCommandPostError(
                        "Another command was triggered in apply record "
                                + apply.getEpicsName()));
                return IdleState;
            }
        }

        @Override
        public State onCarValChange(CarState val) {
            switch(val) {
                case IDLE: {
                    succedCommand(cm);
                    return IdleState;
                }
                case ERROR:{
                    failCommandWithCarError(cm);
                    return IdleState;
                }
                case PAUSED: {
                    pauseCommand(cm);
                    return IdleState;
                }
                default: return this;
            }
        }

        @Override
        public State onCarClidChange(Integer val) {
            if (val == clid) {
                return this;
            } else {
                failCommand(cm, new CaCommandPostError(
                        "Another command was triggered in apply record "
                                + apply.getEpicsName()));
                return IdleState;
            }
        }

        @Override
        public State onTimeout() {
            failCommand(cm, new TimeoutException());
            return IdleState;
        }

    }

    private synchronized void onApplyValChange(Integer val) {
        currentState = currentState.onApplyValChange(val);
        if (currentState.equals(IdleState) && timeoutFuture != null) {
            timeoutFuture.cancel(true);
            timeoutFuture = null;
        }
    }

    private synchronized void onCarClidChange(Integer val) {
        currentState = currentState.onCarClidChange(val);
        if (currentState.equals(IdleState) && timeoutFuture != null) {
            timeoutFuture.cancel(true);
            timeoutFuture = null;
        }
    }

    private synchronized void onCarValChange(CarState carState) {
        currentState = currentState.onCarValChange(carState);
        if (currentState.equals(IdleState) && timeoutFuture != null) {
            timeoutFuture.cancel(true);
            timeoutFuture = null;
        }
    }

    private synchronized void onTimeout() {
        timeoutFuture = null;
        currentState = currentState.onTimeout();
    }

    @Override
    public synchronized boolean isActive() {
        return !currentState.equals(IdleState);
    }

    @Override
    public synchronized void setTimeout(long timeout, TimeUnit timeUnit) {
        this.timeout = timeout;
        this.timeoutUnit = timeUnit;
    }

    private void succedCommand(final CaCommandMonitorImpl cm) {
        executor.execute(new Runnable() {
            @Override
            public void run() {
                cm.completeSuccess();
            }
        });
    }

    private void pauseCommand(final CaCommandMonitorImpl cm) {
        executor.execute(new Runnable() {
            @Override
            public void run() {
                cm.completePause();
            }
        });
    }

    private void failCommand(final CaCommandMonitorImpl cm, final Exception ex) {
        executor.execute(new Runnable() {
            @Override
            public void run() {
                cm.completeFailure(ex);
            }
        });
    }

    private void failCommandWithApplyError(final CaCommandMonitorImpl cm) {
        // I found that if I try to read OMSS or MESS from the same thread that
        // is processing a channel notifications, the reads fails with a
        // timeout. But it works if the read is done later from another thread.
        executor.execute(new Runnable() {
            @Override
            public void run() {
                String msg = null;
                try {
                    msg = apply.getMessValue();
                } catch (CAException | TimeoutException e) {
                    LOG.warning(e.getMessage());
                }
                cm.completeFailure(new CaCommandError(msg));
            }
        });
    }

    private void failCommandWithCarError(final CaCommandMonitorImpl cm) {
        // I found that if I try to read OMSS or MESS from the same thread that
        // is processing a channel notifications, the reads fails with a
        // timeout. But it works if the read is done later from another thread.
        executor.execute(new Runnable() {
            @Override
            public void run() {
                String msg = null;
                try {
                    msg = car.getOmssValue();
                } catch (CAException | TimeoutException e) {
                    LOG.warning(e.getMessage());
                }
                cm.completeFailure(new CaCommandError(msg));
            }
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
            LOG.warning(e.getMessage());
        }
    }

}
