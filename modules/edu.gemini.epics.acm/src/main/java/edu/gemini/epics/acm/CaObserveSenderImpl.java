/*
 * Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import edu.gemini.epics.EpicsReader;
import edu.gemini.epics.EpicsService;
import edu.gemini.epics.ReadOnlyClientEpicsChannel;
import edu.gemini.epics.api.ChannelListener;
import edu.gemini.epics.impl.EpicsReaderImpl;
import gov.aps.jca.CAException;
import gov.aps.jca.TimeoutException;

import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

/**
 * Created by jluhrs on 10/17/17.
 */
public class CaObserveSenderImpl implements CaApplySender {

    private static final Logger LOG = Logger.getLogger(CaObserveSenderImpl.class
            .getName());
    private static final String CAD_MARK_SUFFIX = ".MARK";

    private final String name;
    private final String description;
    
    private EpicsReader epicsReader;
    private final CaApplyRecord apply;
    private final CaCarRecord car;
    private final ReadOnlyClientEpicsChannel<Integer> abortMark;
    private final ReadOnlyClientEpicsChannel<Integer> stopMark;
    
    private long timeout;
    private TimeUnit timeoutUnit;
    private ScheduledExecutorService executor;
    private ScheduledFuture<?> timeoutFuture;
    private final ChannelListener<Integer> valListener;
    private ChannelListener<Integer> carClidListener;
    private ChannelListener<CarState> carValListener;
    private ChannelListener<Integer> abortMarkListener;
    private ChannelListener<Integer> stopMarkListener;
    private CaObserveSenderImpl.State currentState;
    private static final CaObserveSenderImpl.State IdleState = new CaObserveSenderImpl.State() {
        @Override
        public CaObserveSenderImpl.State onApplyValChange(Integer val) {
            return this;
        }

        @Override
        public CaObserveSenderImpl.State onCarValChange(CarState carState) {
            return this;
        }

        @Override
        public CaObserveSenderImpl.State onCarClidChange(Integer val) {
            return this;
        }

        @Override
        public State onStopMarkChange(Integer val) {
            return this;
        }

        @Override
        public State onAbortMarkChange(Integer val) {
            return this;
        }

        @Override
        public CaObserveSenderImpl.State onTimeout() {
            return this;
        }
    };

    private int getMark(ReadOnlyClientEpicsChannel<Integer> ch, int def) {
        if(ch!=null && ch.isValid()) {
            try {
                return ch.getFirst();
            } catch(Exception e) {
                return def;
            }
        } else {
            return def;
        }
    }
    private int getStopMark() {
        return getMark(stopMark, 0);
    }
    private int getAbortMark() {
        return getMark(abortMark, 0);
    }


    public CaObserveSenderImpl(String name, String applyRecord, String carRecord, String stopCmd, String abortCmd,
            String description, EpicsService epicsService) throws CAException {
        super();
        this.name = name;
        this.description = description;
        this.currentState = IdleState;

        epicsReader = new EpicsReaderImpl(epicsService);

        apply = new CaApplyRecord(applyRecord, epicsService);
        apply.registerValListener(valListener = new ChannelListener<Integer>() {
            @Override
            public void valueChanged(String arg0, List<Integer> newVals) {
                if (newVals != null && !newVals.isEmpty()) {
                    CaObserveSenderImpl.this.onApplyValChange(newVals.get(0));
                }
            }
        });
        
        car = new CaCarRecord(carRecord, epicsService);
        car.registerClidListener(carClidListener = new ChannelListener<Integer>() {
            @Override
            public void valueChanged(String arg0, List<Integer> newVals) {
                if (newVals != null && !newVals.isEmpty()) {
                    CaObserveSenderImpl.this.onCarClidChange(newVals.get(0));
                }
            }
        });
        car.registerValListener(carValListener = new ChannelListener<CarState>() {
            @Override
            public void valueChanged(String arg0, List<CarState> newVals) {
                if (newVals != null && !newVals.isEmpty()) {
                    CaObserveSenderImpl.this.onCarValChange(newVals.get(0));
                }
            }
        });

        if(stopCmd!=null && stopCmd.length()>0) {
            stopMark = epicsReader.getIntegerChannel(stopCmd + CAD_MARK_SUFFIX);
            stopMark.registerListener(stopMarkListener = ((String arg0, List<Integer> newVals) -> {
                if (newVals != null && !newVals.isEmpty()) {
                    CaObserveSenderImpl.this.onStopMarkChange(newVals.get(0));
                }
            }));
        } else {
            stopMark = null;
        }

        if(abortCmd!=null && abortCmd.length()>0) {
            abortMark = epicsReader.getIntegerChannel(abortCmd + CAD_MARK_SUFFIX);
            abortMark.registerListener(abortMarkListener = ((String arg0, List<Integer> newVals) -> {
                if (newVals != null && !newVals.isEmpty()) {
                    CaObserveSenderImpl.this.onStopMarkChange(newVals.get(0));
                }
            }));
        } else {
            abortMark = null;
        }

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
        if(stopMark!=null) {
            try {
                stopMark.unRegisterListener(stopMarkListener);
            } catch (CAException e) {
                LOG.warning(e.getMessage());
            }
        }
        if(abortMark!=null) {
            try {
                abortMark.unRegisterListener(abortMarkListener);
            } catch (CAException e) {
                LOG.warning(e.getMessage());
            }
        }

        apply.unbind();
        car.unbind();
    }

    @Override
    public synchronized CaCommandMonitor post() {
        CaCommandMonitorImpl cm = new CaCommandMonitorImpl();
        if (currentState != IdleState) {
            failCommand(cm, new CaCommandInProgress());
        } else {
            currentState = new CaObserveSenderImpl.WaitPreset(cm);

            try {
                apply.setDir(CadDirective.START);
            } catch (CAException | TimeoutException e) {
                failCommand(cm, e);
            }

            if (timeout > 0) {
                timeoutFuture = executor.schedule(new Runnable() {
                    @Override
                    public void run() {
                        CaObserveSenderImpl.this.onTimeout();
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
        CaObserveSenderImpl.State onApplyValChange(Integer val);

        CaObserveSenderImpl.State onCarValChange(CarState carState);

        CaObserveSenderImpl.State onCarClidChange(Integer val);

        CaObserveSenderImpl.State onStopMarkChange(Integer val);

        CaObserveSenderImpl.State onAbortMarkChange(Integer val);

        CaObserveSenderImpl.State onTimeout();
    }

    private final class WaitPreset implements CaObserveSenderImpl.State {
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
        public CaObserveSenderImpl.State onApplyValChange(Integer val) {
            if (val > 0) {
                if (carClid != null && carClid.equals(val)) {
                    if (carVal == CarState.ERROR) {
                        failCommandWithCarError(cm);
                        return IdleState;
                    } else if (carVal == CarState.BUSY) {
                        return new CaObserveSenderImpl.WaitCompletion(cm, val, getStopMark(), getAbortMark());
                    }
                }
                return new CaObserveSenderImpl.WaitStart(cm, val, carVal, carClid);
            } else {
                failCommandWithApplyError(cm);
                return IdleState;
            }
        }

        @Override
        public CaObserveSenderImpl.State onCarValChange(CarState val) {
            return new CaObserveSenderImpl.WaitPreset(cm, val, carClid);
        }

        @Override
        public CaObserveSenderImpl.State onCarClidChange(Integer val) {
            return new CaObserveSenderImpl.WaitPreset(cm, carVal, val);
        }

        @Override
        public CaObserveSenderImpl.State onTimeout() {
            failCommand(cm, new TimeoutException());
            return IdleState;
        }
        @Override
        public State onStopMarkChange(Integer val) {
            return this;
        }

        @Override
        public State onAbortMarkChange(Integer val) {
            return this;
        }

    }

    private final class WaitStart implements CaObserveSenderImpl.State {
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
        public CaObserveSenderImpl.State onApplyValChange(Integer val) {
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
        public CaObserveSenderImpl.State onCarValChange(CarState val) {
            return checkOutConditions(val, carClid);
        }

        @Override
        public CaObserveSenderImpl.State onCarClidChange(Integer val) {
            return checkOutConditions(carState, val);
        }

        @Override
        public CaObserveSenderImpl.State onTimeout() {
            failCommand(cm, new TimeoutException());
            return IdleState;
        }

        @Override
        public State onStopMarkChange(Integer val) {
            return this;
        }

        @Override
        public State onAbortMarkChange(Integer val) {
            return this;
        }

        private CaObserveSenderImpl.State checkOutConditions(CarState carState,
                                                           Integer carClid) {
            if (carClid != null && carClid == clid) {
                if (carState == CarState.ERROR) {
                    failCommandWithCarError(cm);
                    return IdleState;
                }
                if (carState == CarState.BUSY) {
                    return new CaObserveSenderImpl.WaitCompletion(cm, clid, getStopMark(), getAbortMark());
                }
            }
            return new CaObserveSenderImpl.WaitStart(cm, clid, carState, carClid);
        }

    }

    private final class WaitCompletion implements CaObserveSenderImpl.State {
        final CaCommandMonitorImpl cm;
        final int clid;
        final int stopMark;
        final int abortMark;

        WaitCompletion(CaCommandMonitorImpl cm, int clid, int stopMark, int abortMark) {
            this.cm = cm;
            this.clid = clid;
            this.stopMark = stopMark;
            this.abortMark = abortMark;
        }

        @Override
        public CaObserveSenderImpl.State onApplyValChange(Integer val) {
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
        public CaObserveSenderImpl.State onCarValChange(CarState val) {
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
        public CaObserveSenderImpl.State onCarClidChange(Integer val) {
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
        public CaObserveSenderImpl.State onTimeout() {
            failCommand(cm, new TimeoutException());
            return IdleState;
        }

        @Override
        public State onStopMarkChange(Integer val) {
            if(stopMark==2 && val == 0) {
                failCommand(cm, new CaObserveStopped());
                return IdleState;
            } else {
                return new WaitCompletion(cm, clid, val, abortMark);
            }
        }

        @Override
        public State onAbortMarkChange(Integer val) {
            if(abortMark==2 && val == 0) {
                failCommand(cm, new CaObserveAborted());
                return IdleState;
            } else {
                return new WaitCompletion(cm, clid, stopMark, val);
            }
        }

    }

    private synchronized void onApplyValChange(Integer val) {
        currentState = currentState.onApplyValChange(val);
        if (currentState == IdleState && timeoutFuture != null) {
            timeoutFuture.cancel(true);
            timeoutFuture = null;
        }
    }

    private synchronized void onCarClidChange(Integer val) {
        currentState = currentState.onCarClidChange(val);
        if (currentState == IdleState && timeoutFuture != null) {
            timeoutFuture.cancel(true);
            timeoutFuture = null;
        }
    }

    private synchronized void onCarValChange(CarState carState) {
        currentState = currentState.onCarValChange(carState);
        if (currentState == IdleState && timeoutFuture != null) {
            timeoutFuture.cancel(true);
            timeoutFuture = null;
        }
    }

    private synchronized void onStopMarkChange(Integer val) {
        currentState = currentState.onStopMarkChange(val);
        if (currentState == IdleState && timeoutFuture != null) {
            timeoutFuture.cancel(true);
            timeoutFuture = null;
        }
    }

    private synchronized void onAbortMarkChange(Integer val) {
        currentState = currentState.onAbortMarkChange(val);
        if (currentState == IdleState && timeoutFuture != null) {
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
        return currentState != IdleState;
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
