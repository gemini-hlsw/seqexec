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
        executor.execute(() -> listener.onValueChange(vals));
    }

    synchronized public void notifyValueChange(List<T> newVals) {
        for (CaAttributeListener<T> listener : listeners) {
            notifyValueChangeSingle(listener, newVals);
        }
    }

    public void notifyValidityChangeSingle(CaAttributeListener<T> listener, boolean validity) {
        executor.execute(() -> listener.onValidityChange(validity));
    }

    synchronized public void notifyValidityChange(boolean newValidity) {
        for (CaAttributeListener<T> listener : listeners) {
            notifyValidityChangeSingle(listener, newValidity);
        }
    }
}
