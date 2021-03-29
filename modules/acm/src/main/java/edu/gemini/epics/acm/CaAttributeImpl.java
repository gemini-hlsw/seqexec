/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.ImmutableList;

import edu.gemini.epics.EpicsReader;
import edu.gemini.epics.ReadOnlyClientEpicsChannel;
import edu.gemini.epics.api.ChannelListener;
import gov.aps.jca.CAException;

final class CaAttributeImpl<T> implements CaAttribute<T> {
    private static final Logger LOG = LoggerFactory.getLogger(CaAttributeImpl.class.getName());

    private EpicsReader epicsReader;
    private ChannelListener<T> channelListener;
    private ReadOnlyClientEpicsChannel<T> epicsChannel;
    private List<T> values;
    private boolean isValid;
    private final String name;
    private final String channel;
    private final Class<T> type;
    private final String description;
    private final AttributeNotifier<T> notifier;

    private CaAttributeImpl(String name, String channel, Class<T> type,
            String description, EpicsReader epicsReader, ScheduledExecutorService executor) {

        super();
        this.name = name;
        this.channel = channel;
        this.type = type;
        this.description = description;
        this.epicsReader = epicsReader;
        this.notifier = new AttributeNotifier<T>(executor);
    }

    void bind(ReadOnlyClientEpicsChannel<T> epicsChannel)
            throws CAException {
        if(epicsChannel!=null) {
            this.epicsChannel = epicsChannel;
            channelListener = new ChannelListener<T>() {

                @Override
                public void valueChanged(String arg0, List<T> arg1) {
                    if (arg1 != null) {
                        if (arg1.isEmpty()) {
                            setValues(new ArrayList<T>());
                            setValidity(true);
                        } else {
                            if (CaAttributeImpl.this.type.isInstance(arg1.get(0))) {
                                List<T> vals = new ArrayList<>();
                                for (T v : arg1) {
                                    vals.add(v);
                                }

                                setValues(vals);
                                setValidity(true);
                            } else {
                                setValidity(false);
                            }
                        }
                    } else {
                        setValidity(false);
                    }

                }
            };
            this.epicsChannel.registerListener(channelListener);
        } else {
            LOG.warn("Unable to bind to channel " + channel);
        }
    }

    synchronized private void setValues(List<T> newVals) {
        values = newVals;
        notifier.notifyValueChange(values);
    }

    synchronized private void setValidity(boolean newValidity) {
        isValid = newValidity;
        notifier.notifyValidityChange(isValid);
    }

    @Override
    public String name() {
        return name;
    }

    @Override
    synchronized public T value() {
        if (isValid && values != null && values.size() > 0) {
            return values.get(0);
        } else {
            return null;
        }
    }

    @Override
    public boolean valid() {
        return isValid;
    }

    @Override
    synchronized public List<T> values() {
        if (isValid) {
            return ImmutableList.copyOf(values);
        } else {
            return null;
        }
    }

    @Override
    synchronized public void addListener(CaAttributeListener<T> listener) {
        notifier.addListener(listener);
        //Call listener with first value, if there is one.
        if(values() != null) {
            notifier.notifyValueChangeSingle(listener, values());
        }
    }

    @Override
    public void removeListener(CaAttributeListener<T> listener) {
        notifier.removeListener(listener);
    }

    public void unbind() throws CAException {
        assert (epicsReader != null);
        assert (channelListener != null);

        if(epicsChannel!=null) {
            epicsChannel.unRegisterListener(channelListener);
            epicsChannel = null;
        }
        channelListener = null;
        epicsReader = null;
    }

    static CaAttributeImpl<Double> createDoubleAttribute(
            String name, String channel, String description,
            EpicsReader epicsReader, ScheduledExecutorService executor) {
        CaAttributeImpl<Double> attr = new CaAttributeImpl<>(name,
                channel, Double.class, description, epicsReader, executor);
        try {
            attr.bind(epicsReader.getDoubleChannel(channel));
        } catch(Throwable e) {
            LOG.warn(e.getMessage());
        }

        return attr;
    }

    static public CaAttributeImpl<Float> createFloatAttribute(
            String name, String channel, String description,
            EpicsReader epicsReader, ScheduledExecutorService executor) {
        CaAttributeImpl<Float> attr = new CaAttributeImpl<>(name, channel,
                Float.class, description, epicsReader, executor);
        try {
            attr.bind(epicsReader.getFloatChannel(channel));
        } catch(Throwable e) {
            LOG.warn(e.getMessage());
        }

        return attr;
    }

    static public CaAttributeImpl<Integer> createIntegerAttribute(
            String name, String channel, String description,
            EpicsReader epicsReader, ScheduledExecutorService executor) {
        CaAttributeImpl<Integer> attr = new CaAttributeImpl<>(name,
                channel, Integer.class, description, epicsReader, executor);
        try {
            attr.bind(epicsReader.getIntegerChannel(channel));
        } catch(Throwable e) {
            LOG.warn(e.getMessage());
        }

        return attr;
    }

    static public CaAttributeImpl<Short> createShortAttribute(
            String name, String channel, String description,
            EpicsReader epicsReader, ScheduledExecutorService executor) {
        CaAttributeImpl<Short> attr = new CaAttributeImpl<>(name,
                channel, Short.class, description, epicsReader, executor);
        try {
            attr.bind(epicsReader.getShortChannel(channel));
        } catch(Throwable e) {
            LOG.warn(e.getMessage());
        }

        return attr;
    }

    static public CaAttributeImpl<String> createStringAttribute(
            String name, String channel, String description,
            EpicsReader epicsReader, ScheduledExecutorService executor) {
        CaAttributeImpl<String> attr = new CaAttributeImpl<>(name,
                channel, String.class, description, epicsReader, executor);
        try {
            attr.bind(epicsReader.getStringChannel(channel));
        } catch(Throwable e) {
            LOG.warn(e.getMessage());
        }

        return attr;
    }

    static public <T extends Enum<T>> CaAttributeImpl<T> createEnumAttribute(
            String name, String channel, String description, Class<T> enumType,
            EpicsReader epicsReader, ScheduledExecutorService executor) {
        CaAttributeImpl<T> attr = new CaAttributeImpl<>(name, channel,
                enumType, description, epicsReader, executor);
        try {
            attr.bind(epicsReader.getEnumChannel(channel, enumType));
        } catch(Throwable e) {
            LOG.warn(e.getMessage());
        }

        return attr;
    }

    @Override
    public String channel() {
        return channel;
    }

    @Override
    public String description() {
        return description;
    }
}
