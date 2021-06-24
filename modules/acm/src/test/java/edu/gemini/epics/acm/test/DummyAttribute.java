/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm.test;

import edu.gemini.epics.acm.CaAttribute;
import edu.gemini.epics.acm.CaAttributeListener;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

public class DummyAttribute<T> implements CaAttribute<T> {
    private final String name;
    private final String channel;
    private List<T> vals;
    private boolean validity;

    public DummyAttribute(String name, String channel) {
        this.name = name;
        this.channel = channel;
        this.validity = false;
    }

    synchronized public void setValue(T v) {
        this.validity = true;
        this.vals = Collections.singletonList(v);
        notifier.notifyValueChange(this.vals);
    }

    synchronized public void setValidity(boolean v) {
        this.validity = v;
        notifier.notifyValidityChange(this.validity);
    }

    @Override
    public String name() {
        return name;
    }

    @Override
    public String channel() {
        return channel;
    }

    @Override
    public String description() {
        return null;
    }

    @Override
    synchronized public T value() {
        if(validity && vals != null && vals.size() > 0) {
            return vals.get(0);
        } else {
            return null;
        }
    }

    @Override
    synchronized public List<T> values() {
        return vals;
    }

    @Override
    synchronized public boolean valid() {
        return validity;
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
    synchronized public void addListener(CaAttributeListener<T> listener) {
        notifier.addListener(listener);
        if(values()!=null) {
            listener.onValueChange(values());
        }
    }

    @Override
    public void removeListener(CaAttributeListener<T> listener) {
        notifier.removeListener(listener);
    }

}
