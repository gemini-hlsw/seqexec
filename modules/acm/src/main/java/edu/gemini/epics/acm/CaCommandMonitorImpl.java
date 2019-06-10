/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * CaCommandMonitorImpl makes the link between the EPICS code that executes a command and the client that triggers the
 * execution of the command and then checks the command status. Because those operations occur in different threads,
 * objects of this class have to take care of synchronization.
 * <p/>
 * The simplest case is that of the completion callback, that is set from the client thread once and called from
 * (potentially) another thread. This case is simply solved using <code>synchronized</code>.
 * <p/>
 * A more complex case is the command state (with the companion <code>cause</code> attribute that keeps track of error
 * state), because the client must be able to wait on changes on the state attribute. This one is handled with a
 * <code>Lock</code> and <code>Condition</code> object.
 */
public final class CaCommandMonitorImpl implements CaCommandMonitor {

    private final Lock lock;
    private final Condition condition;
    private volatile State currentState;
    private volatile Exception cause;
    private CaCommandListener callback;

    CaCommandMonitorImpl() {
        currentState = State.BUSY;
        lock = new ReentrantLock();
        condition = lock.newCondition();
    }

    @Override
    public Exception error() {
        lock.lock();
        State state = currentState;
        Exception e = cause;
        lock.unlock();
        return (state == State.ERROR) ? e : null;
    }

    @Override
    public boolean isDone() {
        lock.lock();
        State state = currentState;
        lock.unlock();
        return state == State.IDLE || state == State.ERROR;
    }

    @Override
    public void waitDone(long timeout, TimeUnit unit)
            throws TimeoutException, InterruptedException {
        lock.lock();
        try {
            while (!isDone()) {
                if (!condition.await(timeout, unit)) {
                    throw new TimeoutException();
                }
            }
        } finally {
            lock.unlock();
        }
    }

    @Override
    public void waitDone() throws InterruptedException {
        lock.lock();
        try {
            while (!isDone()) {
                condition.await();
            }
        } finally {
            lock.unlock();
        }
    }

    private boolean isActive() {
        return currentState == State.BUSY;
    }

    @Override
    public State waitInactive(long timeout, TimeUnit unit)
            throws TimeoutException, InterruptedException {
        lock.lock();
        State state = currentState;
        try {
            while (isActive()) {
                if (!condition.await(timeout, unit)) {
                    throw new TimeoutException();
                }
            }
            state = currentState;
        } finally {
            lock.unlock();
        }
        return state;
    }

    @Override
    public State waitInactive() throws InterruptedException {
        lock.lock();
        State state = currentState;
        try {
            while (!isActive()) {
                condition.await();
            }
            state = currentState;
        } finally {
            lock.unlock();
        }
        return state;
    }

    @Override
    public State state() {
        return currentState;
    }

    @Override
    public synchronized void setCallback(CaCommandListener cb) {
        callback = cb;
        if (callback != null) {
            lock.lock();
            State state = currentState;
            lock.unlock();
            if (state == State.IDLE) {
                callback.onSuccess();
            } else if (state == State.ERROR) {
                callback.onFailure(cause);
            }
        }
    }

    public synchronized void completeSuccess() {
        lock.lock();
        if (currentState == State.BUSY) {
            currentState = State.IDLE;
            condition.signalAll();
            try {
                if (callback != null) {
                    callback.onSuccess();
                }
            } finally {
                lock.unlock();
            }
        } else {
            lock.unlock();
        }
    }

    public synchronized void completeFailure(Exception cause) {
        lock.lock();
        if (currentState == State.BUSY) {
            this.cause = cause;
            currentState = State.ERROR;
            condition.signalAll();
            try {
                if (callback != null) {
                    callback.onFailure(cause);
                }
            } finally {
                lock.unlock();
            }
        } else {
            lock.unlock();
        }
    }

    public synchronized void completePause() {
        lock.lock();
        if (currentState == State.BUSY) {
            currentState = State.PAUSE;
            condition.signalAll();
            try {
                if (callback != null) {
                    callback.onPause();
                }
            } finally {
                lock.unlock();
            }
        } else {
            lock.unlock();
        }
    }

}
