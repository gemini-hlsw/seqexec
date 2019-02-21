/*
 * Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Created by jluhrs on 10/17/17.
 */
public class CaObserveSenderImpl<C extends Enum<C> & CarStateGeneric> implements CaApplySender {

    private static final Logger LOG = LoggerFactory.getLogger(CaObserveSenderImpl.class.getName());
    private static final String CAD_MARK_SUFFIX = ".MARK";

    private final String name;
    private final String description;

    private EpicsReader epicsReader;
    private final CaApplyRecord apply;
    private final CaCarRecord<C> car;
    private final CaCarRecord<C> observeCar;
    private final ReadOnlyClientEpicsChannel<Short> abortMark;
    private final ReadOnlyClientEpicsChannel<Short> stopMark;

    private final short MRK_PRESET = 2;
    private final short MRK_IDLE = 0;

    private final Boolean trace = false;

    private long timeout;
    private TimeUnit timeoutUnit;
    private ScheduledExecutorService executor;
    private ScheduledFuture<?> timeoutFuture;
    private final ChannelListener<Integer> valListener;
    private ChannelListener<Integer> carClidListener;
    private ChannelListener<C> carValListener;
    private ChannelListener<C> observeCarValListener;
    private ChannelListener<Short> abortMarkListener;
    private ChannelListener<Short> stopMarkListener;
    private CaObserveSenderImpl.ApplyState currentState;

    public CaObserveSenderImpl(String name, String applyRecord, String carRecord, String observeCarRecord,
                               String stopCmd, String abortCmd, String description, Class<C> carClass, EpicsService epicsService)
            throws CAException {
        super();
        this.name = name;
        this.description = description;
        this.currentState = idleState;

        epicsReader = new EpicsReaderImpl(epicsService);

        apply = new CaApplyRecord(applyRecord, epicsService);
        // apply.VAL int > 0
        apply.registerValListener(valListener = (String arg0, List<Integer> newVals) -> {
            if (newVals != null && !newVals.isEmpty()) {
                CaObserveSenderImpl.this.onApplyValChange(newVals.get(0));
            }
        });

        car = new CaCarRecord<C>(carRecord, carClass, epicsService);
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

        observeCar = new CaCarRecord<C>(observeCarRecord, carClass, epicsService);
        // observeC.VAL BUSY/IDLE/PAUSED
        observeCar.registerValListener(observeCarValListener = (String arg0, List<C> newVals) -> {
            if (newVals != null && !newVals.isEmpty()) {
                CaObserveSenderImpl.this.onObserveCarValChange(newVals.get(0));
            }
        });

        ReadOnlyClientEpicsChannel<Short> stopMark = null;
        if(stopCmd!=null && stopCmd.length()>0) {
            try {
                stopMark = epicsReader.getShortChannel(stopCmd + CAD_MARK_SUFFIX);
            } catch(Throwable e) {
                LOG.warn(e.getMessage());
            }
            if(stopMark!=null) {
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
        if(abortCmd!=null && abortCmd.length()>0) {
            try {
                abortMark = epicsReader.getShortChannel(abortCmd + CAD_MARK_SUFFIX);
            } catch(Throwable e) {
                LOG.warn(e.getMessage());
            }
            if(abortMark!=null) {
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
        if(stopMark!=null) {
            try {
                stopMark.unRegisterListener(stopMarkListener);
            } catch (CAException e) {
                LOG.warn(e.getMessage());
            }
        }
        if(abortMark!=null) {
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
            System.out.println("POST !");
            System.out.println(currentState);
        if (!currentState.equals(idleState)) {
            failCommand(cm, new CaCommandInProgress());
        } else {
            currentState = new CaObserveSenderImpl.WaitApplyPreset(cm);

            try {
                apply.setDir(CadDirective.START);
            } catch (CAException | TimeoutException e) {
                failCommand(cm, e);
            }

            if (timeout > 0) {
                timeoutFuture = executor.schedule(() ->  CaObserveSenderImpl.this.onTimeout(), timeout, timeoutUnit);
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

    // Used on a FSM tracking the state of apply
    private interface ApplyState {
        String signature();

        CaObserveSenderImpl.ApplyState onApplyValChange(Integer val);

        CaObserveSenderImpl.ApplyState onCarValChange(CarStateGeneric carState);

        CaObserveSenderImpl.ApplyState onCarClidChange(Integer val);

        ObserveState currentObserveState();

        CaObserveSenderImpl.ApplyState copyWithObserveState(ObserveState observeState);

        default CaObserveSenderImpl.ApplyState onStopMarkChange(Short val) {
          return copyWithObserveState(currentObserveState().onStopMarkChange(val));
        }

        default CaObserveSenderImpl.ApplyState onAbortMarkChange(Short val) {
          return copyWithObserveState(currentObserveState().onStopMarkChange(val));
        }

        default CaObserveSenderImpl.ApplyState onObserveCarValChange(CarStateGeneric carState) {
          return copyWithObserveState(currentObserveState().onObserveCarValChange(carState));
        }

        CaObserveSenderImpl.ApplyState onTimeout();
    }

    // Used on a FSM tracking the state of observe
    private interface ObserveState {
        String signature();

        ObserveState onObserveCarValChange(CarStateGeneric carState);

        CaObserveSenderImpl.ObserveState onStopMarkChange(Short val);

        CaObserveSenderImpl.ObserveState onAbortMarkChange(Short val);

        boolean isDone();
    }

    // Initial state before listening for anything
    private final static class ObserveIdleState implements ObserveState {

        ObserveIdleState() {
        }

        public String signature() {return "ObserveIdle";}

        public ObserveState onObserveCarValChange(CarStateGeneric carState) {
          return this;
        }

        public CaObserveSenderImpl.ObserveState onStopMarkChange(Short val) {
          return this;
        }

        public CaObserveSenderImpl.ObserveState onAbortMarkChange(Short val) {
          return this;
        }

        public boolean isDone() {return false;}
    }

    private ObserveState idleObserveState = new ObserveIdleState();

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

        public String signature() {return "ObserveWaitBusy";}

        public ObserveState onObserveCarValChange(CarStateGeneric carState) {
          if (carState.isBusy()) {
              // Now we are busy, let's go to wait for idle
              return new ObserveWaitIdle(carState, cm);
          } else if (carState.isError()) {
              // Signal an error
              failCommandWithCarError(cm);
              return idleObserveState;
          } else {
              // No change, preserve the car
              return new ObserveWaitBusy(carState, cm);
          }
        }

        public CaObserveSenderImpl.ObserveState onStopMarkChange(Short val) {
          return this;
        }

        public CaObserveSenderImpl.ObserveState onAbortMarkChange(Short val) {
          return this;
        }

        public boolean isDone() {return false;}
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
        public String signature() { return "ObserveWaitIdle"; }

        @Override
        public ObserveState onObserveCarValChange(CarStateGeneric carState) {
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
            else if (observeCarState.isPaused()){
                pauseCommand(cm);
                return this;//new ObserveIdleState(cm);
            }
            else {
                // Should never get here
                return this;
            }
        }

        @Override
        public CaObserveSenderImpl.ObserveState onStopMarkChange(Short val) {
          return this;
        }

        @Override
        public CaObserveSenderImpl.ObserveState onAbortMarkChange(Short val) {
          return this;
        }

        @Override
        public boolean isDone() { return false; }
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
        public String signature() { return "ObserveDone"; }

        @Override
        public ObserveState onObserveCarValChange(CarStateGeneric carState) {
          return this;}

        @Override
        public CaObserveSenderImpl.ObserveState onStopMarkChange(Short val) {
          return this;
        }

        @Override
        public CaObserveSenderImpl.ObserveState onAbortMarkChange(Short val) {
          return this;
        }

        @Override
        public boolean isDone() { return true; }
    };

    // Initial state before any channel has changed
    private final class IdleState implements ApplyState {
        IdleState() {
        }

        @Override
        public String signature() { return "idleState"; }

        @Override
        public CaObserveSenderImpl.ApplyState onApplyValChange(Integer val) {
            return this;
        }

        @Override
        public CaObserveSenderImpl.ApplyState onCarValChange(CarStateGeneric carState) {
            return this;
        }

        @Override
        public CaObserveSenderImpl.ApplyState onCarClidChange(Integer val) { return this; }

        @Override
        public CaObserveSenderImpl.ApplyState onTimeout() {
            return this;
        }

        @Override
        public CaObserveSenderImpl.ApplyState copyWithObserveState(ObserveState observeState) {
          return this;
        }

        @Override
        public ObserveState currentObserveState() {
          return idleObserveState;
        }

    };

    private IdleState idleState = new IdleState();

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

        private WaitApplyPreset(CaCommandMonitorImpl cm, CarStateGeneric carState, Integer carClid, ObserveState observeState) {
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
        public CaObserveSenderImpl.ApplyState onApplyValChange(Integer val) {
            if (val > 0) {
                if (val.equals(carClid) && carState != null) {
                    if (carState.isError()) {
                        failCommandWithCarError(cm);
                        return idleState;
                    } else if (carState.isBusy()) {
                        return new CaObserveSenderImpl.WaitApplyBusy(cm, val, carState, observeState);
                    }
                }
                return new CaObserveSenderImpl.WaitApplyBusy(cm, val, carState, observeState);
            } else {
                failCommandWithApplyError(cm);
                return idleState;
            }
        }

        @Override
        public CaObserveSenderImpl.ApplyState onCarValChange(CarStateGeneric val) {
            return new CaObserveSenderImpl.WaitApplyPreset(cm, val, carClid, observeState);
        }

        @Override
        public CaObserveSenderImpl.ApplyState onCarClidChange(Integer val) {
            return new CaObserveSenderImpl.WaitApplyPreset(cm, carState, val, observeState);
        }

        @Override
        public ApplyState onObserveCarValChange(CarStateGeneric val)  {
            return new CaObserveSenderImpl.WaitApplyPreset(cm, carState, carClid, observeState.onObserveCarValChange(val));
        }

        @Override
        public CaObserveSenderImpl.ApplyState onTimeout() {
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

        @Override
        public CaObserveSenderImpl.ApplyState copyWithObserveState(ObserveState observeState) {
            return new CaObserveSenderImpl.WaitApplyPreset(cm, carState, carClid, observeState);
        }

        @Override
        public ObserveState currentObserveState() {
          return observeState;
        }

    }
    // private final class WaitPreset implements CaObserveSenderImpl.State {
    //     final CaCommandMonitorImpl cm;
    //     final CarStateGeneric carState;
    //     final CarStateGeneric observeCarState;
    //     final Integer carClid;
    //     final ObserveState observeState;
    //
    //     WaitPreset(CaCommandMonitorImpl cm) {
    //         this.cm = cm;
    //         this.carState = null;
    //         this.carClid = null;
    //         this.observeCarState = null;
    //         this.observeState = observeStart;
    //     }
    //
    //     private WaitPreset(CaCommandMonitorImpl cm, CarStateGeneric carState, Integer carClid, CarStateGeneric observeCarState, ObserveState observeState) {
    //         this.cm = cm;
    //         this.carState = carState;
    //         this.carClid = carClid;
    //         this.observeCarState = observeCarState;
    //         this.observeState = observeState;
    //     }
    //
    //     @Override
    //     public String signature() {
    //         return "WaitPreset(carState = " + carState + ", carClid = " + carClid +
    //                 ", observeCarState = " + observeCarState + ", observeState = " + observeState.signature() + ")";
    //     }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onApplyValChange(Integer val) {
    //         if (val > 0) {
    //             if (val.equals(carClid) && carState != null) {
    //                 if (carState.isError()) {
    //                     failCommandWithCarError(cm);
    //                     return idleState;
    //                 } else if (carState.isBusy()) {
    //                     return new CaObserveSenderImpl.WaitCompletion(cm, val,  observeState);
    //                 }
    //             }
    //             return new CaObserveSenderImpl.WaitCompletion(cm, val, carState, carClid, observeCarState, observeState);
    //         } else {
    //             failCommandWithApplyError(cm);
    //             return idleState;
    //         }
    //     }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onCarValChange(CarStateGeneric val) {
    //         return new CaObserveSenderImpl.WaitPreset(cm, val, carClid, observeCarState, observeState);
    //     }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onCarClidChange(Integer val) {
    //         return new CaObserveSenderImpl.WaitPreset(cm, carState, val, observeCarState, observeState);
    //     }
    //
    //     @Override
    //     public State onObserveCarValChange(CarStateGeneric val)  {
    //         return new CaObserveSenderImpl.WaitPreset(cm, carState, carClid, val, observeState.onObserveCarValChange(val));
    //     }
    //
    //     @Override
    //     public CaObserveSenderImpl.ApplyState onTimeout() {
    //         failCommand(cm, new TimeoutException());
    //         return idleState;
    //     }
    //     @Override
    //     public State onStopMarkChange(Short val) {
    //         return this;
    //     }
    //
    //     @Override
    //     public State onAbortMarkChange(Short val) {
    //         return this;
    //     }
    //
    // }

    // private final class WaitStart implements CaObserveSenderImpl.State {
    //     final CaCommandMonitorImpl cm;
    //     final int clid;
    //     final Integer carClid;
    //     final CarStateGeneric carState;
    //     final CarStateGeneric observeCarState;
    //
    //     WaitStart(CaCommandMonitorImpl cm, int clid, CarStateGeneric carState,
    //             Integer carClid, CarStateGeneric observeCarState) {
    //         this.cm = cm;
    //         this.clid = clid;
    //         this.carState = carState;
    //         this.carClid = carClid;
    //         this.observeCarState = observeCarState;
    //     }
    //
    //     @Override
    //     public String signature() {
    //         return "WaitStart(clid = " + clid + "carState = " + carState + ", carClid = " + carClid +
    //                 ", observeCarState = " + observeCarState + ")";
    //     }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onApplyValChange(Integer val) {
    //         if (val == clid) {
    //             return this;
    //         } else {
    //             failCommand(cm, new CaCommandPostError(
    //                     "Another command was triggered in apply record "
    //                             + apply.getEpicsName()));
    //             return idleState;
    //         }
    //     }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onCarValChange(CarStateGeneric val) {
    //         return checkOutConditions(val, carClid);
    //     }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onCarClidChange(Integer val) {
    //         return checkOutConditions(carState, val);
    //     }
    //
    //     @Override
    //     public CaObserveSenderImpl.ApplyState onTimeout() {
    //         failCommand(cm, new TimeoutException());
    //         return idleState;
    //     }
    //
    //     @Override
    //     public State onObserveCarValChange(CarStateGeneric val) {
    //         return new CaObserveSenderImpl.WaitStart(cm, clid, carState, carClid, val);
    //     }
    //
    //     @Override
    //     public State onStopMarkChange(Short val) {
    //         return this;
    //     }
    //
    //     @Override
    //     public State onAbortMarkChange(Short val) {
    //         return this;
    //     }
    //
    //     private CaObserveSenderImpl.State checkOutConditions(CarStateGeneric carState,
    //                                                        Integer carClid) {
    //         if (carState != null && carClid != null && carClid == clid) {
    //             if (carState.isError()) {
    //                 failCommandWithCarError(cm);
    //                 return idleState;
    //             }
    //             if (carState.isBusy()) {
    //                 return new CaObserveSenderImpl.WaitCompletion(cm, clid, observeCarState, observeState);
    //             }
    //         }
    //         return new CaObserveSenderImpl.WaitStart(cm, clid, carState, carClid, observeCarState);
    //     }
    //
    // }

    // In this stat we wait for applyC to turn to busy
    private final class WaitApplyBusy implements ApplyState {
        final CaCommandMonitorImpl cm;
        final int clid;
        final CarStateGeneric carState;
        final ObserveState observeState;

        WaitApplyBusy(CaCommandMonitorImpl cm, int clid, CarStateGeneric carState, ObserveState observeState) {
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
        public ApplyState onApplyValChange(Integer val) {
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
        public ApplyState onCarValChange(CarStateGeneric val) {
            if (val.isBusy()) {
                return new WaitApplyIdle(cm, clid, carState, observeState);
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
        public ApplyState onCarClidChange(Integer val) {
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
        public ApplyState onObserveCarValChange(CarStateGeneric val) {
            return new WaitApplyBusy(cm, clid, carState, observeState.onObserveCarValChange(val));
        }

        @Override
        public ApplyState onStopMarkChange(Short val) { return this; }

        @Override
        public ApplyState onAbortMarkChange(Short val) { return this; }

        @Override
        public ApplyState onTimeout() {
            failCommand(cm, new TimeoutException());
            return idleState;
        }

        @Override
        public CaObserveSenderImpl.ApplyState copyWithObserveState(ObserveState observeState) {
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

        WaitApplyIdle(CaCommandMonitorImpl cm, int clid, CarStateGeneric carState, ObserveState observeState) {
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
        public ApplyState onApplyValChange(Integer val) {
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
        public ApplyState onCarValChange(CarStateGeneric val) {
            if (val.isIdle()) {
                return checkOutCompletion(val, clid);
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
        public ApplyState onCarClidChange(Integer val) {
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
        public ApplyState onObserveCarValChange(CarStateGeneric val) {
            return new WaitApplyIdle(cm, clid, carState, observeState.onObserveCarValChange(val));
        }

        @Override
        public ApplyState onStopMarkChange(Short val) { return this; }

        @Override
        public ApplyState onAbortMarkChange(Short val) { return this; }

        @Override
        public ApplyState onTimeout() {
            failCommand(cm, new TimeoutException());
            return idleState;
        }

        private CaObserveSenderImpl.ApplyState checkOutCompletion(CarStateGeneric carState,
                                                           Integer carClid) {
            if (carState != null && carClid != null && carClid == clid) {
                if (carState.isError()) {
                    failCommandWithCarError(cm);
                    return idleState;
                }
                if (observeState.isDone()) {
                    succedCommand(cm);
                    return new WaitApplyIdle(cm, clid, carState, observeState);
                }
            }
            return new WaitApplyIdle(cm, clid, carState, observeState);
        }

        @Override
        public CaObserveSenderImpl.ApplyState copyWithObserveState(ObserveState observeState) {
            return new WaitApplyBusy(cm, clid, carState, observeState);
        }

        @Override
        public ObserveState currentObserveState() {
          return observeState;
        }
    }
    // private final class WaitCompletion implements State {
    //     final CaCommandMonitorImpl cm;
    //     final int clid;
    //     // final CarStateGeneric observeCarState;
    //     final ObserveState observeState;
    //
    //     WaitCompletion(CaCommandMonitorImpl cm, int clid, /*CarStateGeneric observeCarState,*/ ObserveState observeState) {
    //         this.cm = cm;
    //         this.clid = clid;
    //         // this.observeCarState = observeCarState;
    //         this.observeState = observeState;
    //     }
    //
    //     @Override
    //     public String signature() {
    //         return "WaitCompletion(clid = " + clid + ", observeState = " + observeState.signature() + ")";
    //     }
    //
    //     @Override
    //     public State onApplyValChange(Integer val) {
    //         if (val == clid) {
    //             return this;
    //         } else {
    //             failCommand(cm, new CaCommandPostError(
    //                     "Another command was triggered in apply record "
    //                             + apply.getEpicsName()));
    //             return idleState;
    //         }
    //     }
    //
    //     @Override
    //     public State onCarValChange(CarStateGeneric val) {
    //         if (val.isIdle()) {
    //             if (observeCarState == null || observeCarState.isIdle()) {
    //                 return new WaitObserveStart(cm);
    //             }
    //             else if (observeCarState.isBusy()) {
    //                 return new WaitObserveCompletion(cm);
    //             }
    //             else if (observeCarState.isError()) {
    //                 failCommandWithObserveCarError(cm);
    //                 return idleState;
    //             }
    //             else if (observeCarState.isPaused()){
    //                 pauseCommand(cm);
    //                 return idleState;
    //             }
    //             else {
    //                 // Should never get here
    //                 return this;
    //             }
    //         }
    //         else if (val.isError()) {
    //             failCommandWithCarError(cm);
    //             return idleState;
    //         }
    //         else {
    //             return this;
    //         }
    //     }
    //
    //
    //     @Override
    //     public State onCarClidChange(Integer val) {
    //         if (val == clid) {
    //             return this;
    //         } else {
    //             failCommand(cm, new CaCommandPostError(
    //                     "Another command was triggered in apply record "
    //                             + apply.getEpicsName()));
    //             return idleState;
    //         }
    //     }
    //
    //     @Override
    //     public State onObserveCarValChange(CarStateGeneric val) {
    //         return new WaitCompletion(cm, clid, val, observeState);
    //     }
    //
    //     @Override
    //     public State onStopMarkChange(Short val) { return this; }
    //
    //     @Override
    //     public State onAbortMarkChange(Short val) { return this; }
    //
    //     @Override
    //     public State onTimeout() {
    //         failCommand(cm, new TimeoutException());
    //         return idleState;
    //     }
    //
    // }

    // private final class WaitObserveStart implements CaObserveSenderImpl.State {
    //     final CaCommandMonitorImpl cm;
    //
    //     WaitObserveStart(CaCommandMonitorImpl cm) {
    //         this.cm = cm;
    //     }
    //
    //     @Override
    //     public String signature() {
    //         return "WaitObserveStart";
    //     }
    //
    //     @Override
    //     public State onApplyValChange(Integer val) { return this; }
    //
    //     @Override
    //     public State onCarValChange(CarStateGeneric carState) { return this; }
    //
    //     @Override
    //     public State onCarClidChange(Integer val) { return this; }
    //
    //     @Override
    //     public State onObserveCarValChange(CarStateGeneric val) {
    //         if(val.isBusy()){
    //             return new WaitObserveCompletion(cm);
    //         }
    //         else if(val.isError()){
    //             failCommandWithObserveCarError(cm);
    //             return idleState;
    //         } else if(val.isPaused()){
    //             pauseCommand(cm);
    //             return idleState;
    //         }
    //         else {
    //             return this;
    //         }
    //     }
    //
    //     @Override
    //     public State onStopMarkChange(Short val) { return this; }
    //
    //     @Override
    //     public State onAbortMarkChange(Short val) { return this; }
    //
    //     @Override
    //     public State onTimeout() {
    //         failCommand(cm, new TimeoutException());
    //         return idleState;
    //     }
    // }

    // private final class WaitObserveCompletion implements State {
    //     final CaCommandMonitorImpl cm;
    //     final short stopMark;
    //     final short abortMark;
    //
    //     WaitObserveCompletion(CaCommandMonitorImpl cm) {
    //         this.cm = cm;
    //         this.stopMark = MRK_IDLE;
    //         this.abortMark = MRK_IDLE;
    //     }
    //
    //     private WaitObserveCompletion(CaCommandMonitorImpl cm, short stopMark, short abortMark) {
    //         this.cm = cm;
    //         this.stopMark = stopMark;
    //         this.abortMark = abortMark;
    //     }
    //
    //     @Override
    //     public String signature() {
    //         return "WaitObserveCompletion( stopMark = " + stopMark + ", abortMark = " + abortMark + ")";
    //     }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onApplyValChange(Integer val) {
    //         return this;
    //     }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onCarValChange(CarStateGeneric val) { return this; }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onCarClidChange(Integer val) { return this; }
    //
    //     @Override
    //     public State onObserveCarValChange(CarStateGeneric val) {
    //
    //         if(val.isIdle()) {
    //             succedCommand(cm);
    //             return idleState;
    //         }
    //         else if(val.isError()) {
    //             failCommandWithObserveCarError(cm);
    //             return idleState;
    //         }
    //         else if(val.isPaused()) {
    //             pauseCommand(cm);
    //             return idleState;
    //         }
    //         else {
    //             return this;
    //         }
    //     }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onTimeout() {
    //         failCommand(cm, new TimeoutException());
    //         return idleState;
    //     }
    //
    //     @Override
    //     public State onStopMarkChange(Short val) {
    //         if(stopMark == MRK_PRESET && val == MRK_IDLE) {
    //             return new WaitStopCompletion(cm);
    //         } else {
    //             return new WaitObserveCompletion(cm, val, abortMark);
    //         }
    //     }
    //
    //     @Override
    //     public State onAbortMarkChange(Short val) {
    //         if(abortMark == MRK_PRESET && val == MRK_IDLE) {
    //             return new WaitAbortCompletion(cm);
    //         } else {
    //             return new WaitObserveCompletion(cm, stopMark, val);
    //         }
    //     }
    //
    // }

    // private final class WaitStopCompletion implements CaObserveSenderImpl.State {
    //     final CaCommandMonitorImpl cm;
    //
    //     WaitStopCompletion(CaCommandMonitorImpl cm) {
    //         this.cm = cm;
    //     }
    //
    //     @Override
    //     public String signature() {
    //         return "WaitStopCompletion( stopMark = " + stopMark + ", abortMark = " + abortMark + ")";
    //     }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onApplyValChange(Integer val) {
    //         return this;
    //     }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onCarValChange(CarStateGeneric val) { return this; }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onCarClidChange(Integer val) { return this; }
    //
    //     @Override
    //     public State onObserveCarValChange(CarStateGeneric val) {
    //         if(val.isIdle()) {
    //             failCommand(cm, new CaObserveStopped());
    //             return idleState;
    //         }
    //         else if(val.isError()) {
    //             failCommandWithCarError(cm);
    //             return idleState;
    //         }
    //         else if(val.isPaused()) {
    //             pauseCommand(cm);
    //             return idleState;
    //         }
    //         else {
    //             return this;
    //         }
    //     }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onTimeout() {
    //         failCommand(cm, new TimeoutException());
    //         return idleState;
    //     }
    //
    //     @Override
    //     public State onStopMarkChange(Short val) {
    //         return this;
    //     }
    //
    //     @Override
    //     public State onAbortMarkChange(Short val) {
    //         return this;
    //     }
    //
    // }

    // private final class WaitAbortCompletion implements CaObserveSenderImpl.State {
    //     final CaCommandMonitorImpl cm;
    //
    //     WaitAbortCompletion(CaCommandMonitorImpl cm) {
    //         this.cm = cm;
    //     }
    //
    //     @Override
    //     public String signature() {
    //         return "WaitAbortCompletion( stopMark = " + stopMark + ", abortMark = " + abortMark + ")";
    //     }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onApplyValChange(Integer val) {
    //         return this;
    //     }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onCarValChange(CarStateGeneric val)  {
    //                 return this;
    //             }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onCarClidChange(Integer val) { return this; }
    //
    //     @Override
    //     public State onObserveCarValChange(CarStateGeneric val) {
    //         if(val.isIdle()) {
    //             failCommand(cm, new CaObserveAborted());
    //             return idleState;
    //         }
    //         else if(val.isError()) {
    //             failCommandWithObserveCarError(cm);
    //             return idleState;
    //         }
    //         else if(val.isPaused()) {
    //             pauseCommand(cm);
    //             return idleState;
    //         }
    //         else {
    //             return this;
    //         }
    //     }
    //
    //     @Override
    //     public CaObserveSenderImpl.State onTimeout() {
    //         failCommand(cm, new TimeoutException());
    //         return idleState;
    //     }
    //
    //     @Override
    //     public State onStopMarkChange(Short val) {
    //         return this;
    //     }
    //
    //     @Override
    //     public State onAbortMarkChange(Short val) {
    //         return this;
    //     }
    //
    // }

    private synchronized void onApplyValChange(Integer val) {
        if (val != null) {
            ApplyState oldState = currentState;
            currentState = currentState.onApplyValChange(val);
            if (currentState.equals(idleState) && timeoutFuture != null) {
                timeoutFuture.cancel(true);
                timeoutFuture = null;
            }
            if(trace) LOG.debug("onApplyValChange(" + val + "): " + oldState.signature() + " -> " + currentState.signature());
        }
    }

    private synchronized void onCarClidChange(Integer val) {
        if (val != null) {
            ApplyState oldState = currentState;
            currentState = currentState.onCarClidChange(val);
            if (currentState.equals(idleState) && timeoutFuture != null) {
                timeoutFuture.cancel(true);
                timeoutFuture = null;
            }
            if(trace) LOG.debug("onCarClidChange(" + val + "): " + oldState.signature() + " -> " + currentState.signature());
        }
    }

    private synchronized void onCarValChange(C carState) {
        if (carState != null) {
            ApplyState oldState = currentState;
            currentState = currentState.onCarValChange(carState);
            if (currentState.equals(idleState) && timeoutFuture != null) {
                timeoutFuture.cancel(true);
                timeoutFuture = null;
            }
            if(trace) LOG.debug("onCarValChange(" + carState + "): " + oldState.signature() + " -> " + currentState.signature());
        }
    }

    private synchronized void onObserveCarValChange(C carState) {
        if (carState != null) {
            ApplyState oldState = currentState;
            currentState = currentState.onObserveCarValChange(carState);
            if (currentState.equals(idleState) && timeoutFuture != null) {
                timeoutFuture.cancel(true);
                timeoutFuture = null;
            }
            if(trace) LOG.debug("onObserveCarValChange(" + carState + "): " + oldState.signature() + " -> " + currentState.signature());
        }
    }

    private synchronized void onStopMarkChange(Short val) {
        if (val != null) {
            ApplyState oldState = currentState;
            currentState = currentState.onStopMarkChange(val);
            if (currentState.equals(idleState) && timeoutFuture != null) {
                timeoutFuture.cancel(true);
                timeoutFuture = null;
            }
            if(trace) LOG.debug("onStopMarkChange(" + val + "): " + oldState.signature() + " -> " + currentState.signature());
        }
    }

    private synchronized void onAbortMarkChange(Short val) {
        if (val != null) {
            ApplyState oldState = currentState;
            currentState = currentState.onAbortMarkChange(val);
            if (currentState.equals(idleState) && timeoutFuture != null) {
                timeoutFuture.cancel(true);
                timeoutFuture = null;
            }
            if(trace) LOG.debug("onAbortMarkChange(" + val + "): " + oldState.signature() + " -> " + currentState.signature());
        }
    }

    private synchronized void onTimeout() {
        timeoutFuture = null;
        ApplyState oldState = currentState;
        currentState = currentState.onTimeout();
        if(trace) LOG.debug("onTimeout: " + oldState.signature() + " -> " + currentState.signature());
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
