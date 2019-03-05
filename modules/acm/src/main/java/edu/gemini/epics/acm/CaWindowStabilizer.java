/*
 * Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import java.time.Duration;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class CaWindowStabilizer<T> implements CaAttribute<T> {

    private final CaAttribute<T> sa;
    private final Duration settleTime;
    private final ScheduledExecutorService executor;
    private ScheduledFuture<?> timeoutFuture;
    private final CaAttributeListener<T> valListener;
    private T filteredVal;
    private T lastVal;

    public CaWindowStabilizer(CaAttribute<T> sa, Duration settleTime) {
        this.sa = sa;
        this.settleTime = settleTime;
        lastVal = filteredVal = sa.value();

        executor = new ScheduledThreadPoolExecutor(2);

        sa.addListener(valListener = new CaAttributeListener<T>() {
            @Override
            public void onValueChange(List<T> newVals) {
                if(newVals != null && !newVals.isEmpty()) {
                    CaWindowStabilizer.this.onValChange(newVals.get(0));
                }
            }

            @Override
            public void onValidityChange(boolean newValidity) {
                notifier.notifyValidityChange(newValidity);
            }
        });
    }

    private synchronized void onValChange(T val) {

        if (val != null && val != lastVal) {
            if (timeoutFuture != null) {
                timeoutFuture.cancel(true);
            }
            lastVal = val;
            timeoutFuture = executor.schedule(new Runnable() {
                @Override
                public void run() {
                    CaWindowStabilizer.this.onTimeout();
                }
            }, settleTime.toMillis(), TimeUnit.MILLISECONDS);
        }
    }

    // If the timeout triggers, it means that the last value has not changed in `settleTime`
    private synchronized void onTimeout() {
        timeoutFuture = null;
        filteredVal = lastVal;
        notifier.notifyValueChange(values());
    }

    void unbind() {

        executor.shutdown();

        sa.removeListener(valListener);
    }

    @Override
    public String name() {
        return null;
    }

    @Override
    public String channel() {
        return null;
    }

    @Override
    public String description() {
        return null;
    }

    @Override
    public T value() {
        return filteredVal;
    }

    @Override
    public List<T> values() {
        if(filteredVal != null)
            return Arrays.asList(filteredVal);
        else
            return Arrays.asList();
    }

    @Override
    public boolean valid() {
        return sa.valid();
    }

    private final class Notifier {
        private final List<CaAttributeListener<T>> listeners = new LinkedList<>();

        synchronized public void addListener(CaAttributeListener<T> listener) {
            if (!listeners.contains(listener)) {
                listeners.add(listener);
            }
        }

        synchronized public void removeListener(CaAttributeListener<T> listener) {
            listeners.remove(listener);
        }

        synchronized public void notifyValueChange(List<T> newVals) {
            for (CaAttributeListener<T> listener : listeners) {
                listener.onValueChange(newVals);
            }
        }

        synchronized public void notifyValidityChange(boolean newValidity) {
            for (CaAttributeListener<T> listener : listeners) {
                listener.onValidityChange(newValidity);
            }
        }
    }

    private final Notifier notifier = new Notifier();

    @Override
    public void addListener(CaAttributeListener<T> listener) {
        notifier.addListener(listener);
    }

    @Override
    public void removeListener(CaAttributeListener<T> listener) {
        notifier.removeListener(listener);
    }

    public CaWindowStabilizer<T> reset() {
        filteredVal = lastVal = null;
        return this;
    }

}
