/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;

public class AttributeNotifier<T> {

    private final ScheduledExecutorService executor;
    private final List<CaAttributeListener<T>> listeners = new LinkedList<>();

    public AttributeNotifier(ScheduledExecutorService executor) {
        this.executor = executor;
    }

    synchronized public void addListener(CaAttributeListener<T> listener) {
        if (!listeners.contains(listener)) {
            listeners.add(listener);
        }
    }

    synchronized public void removeListener(CaAttributeListener<T> listener) {
        listeners.remove(listener);
    }

    public void notifyValueChangeSingle(CaAttributeListener<T> listener, List<T> vals) {
        executor.execute(() -> {
            synchronized (AttributeNotifier.this) {
                // Check that the listener has not been removed between the call to notifyValueChangeSingle and
                // the deferred execution of the callback.
                if(AttributeNotifier.this.listeners.contains(listener)) {
                    listener.onValueChange(vals);
                }
            }
        });
    }

    synchronized public void notifyValueChange(List<T> newVals) {
        for (CaAttributeListener<T> listener : listeners) {
            notifyValueChangeSingle(listener, newVals);
        }
    }

    public void notifyValidityChangeSingle(CaAttributeListener<T> listener, boolean validity) {
        executor.execute(() -> {
            synchronized (AttributeNotifier.this) {
                // Check that the listener has not been removed between the call to notifyValidityChangeSingle and
                // the deferred execution of the callback.
                if(AttributeNotifier.this.listeners.contains(listener)) {
                    listener.onValidityChange(validity);
                }
            }
        });
    }

    synchronized public void notifyValidityChange(boolean newValidity) {
        for (CaAttributeListener<T> listener : listeners) {
            notifyValidityChangeSingle(listener, newValidity);
        }
    }
}
