/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import edu.gemini.epics.EpicsWriter;
import edu.gemini.epics.ReadWriteClientEpicsChannel;
import gov.aps.jca.CAException;
import gov.aps.jca.TimeoutException;

abstract class CaParameterImpl<T> implements CaParameter<T> {

    private static final Logger LOG = LoggerFactory.getLogger(CaParameterImpl.class.getName());

    private final String name;
    private final String channel;
    private final String description;
    private EpicsWriter epicsWriter;
//    private ReadWriteClientEpicsChannel<String> rwChannel;
    private T value;

    private CaParameterImpl(String name, String channel, String description,
                            EpicsWriter epicsWriter) {
        this.name = name;
        this.channel = channel;
        this.description = description;
        this.epicsWriter = epicsWriter;
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
    public T value() {
        return value;
    }

    @Override
    public String description() {
        return description;
    }

    @Override
    public void set(T value) throws CAException, TimeoutException {
        this.value = value;

        write(value);
    }

    public void unbind() {
        unbindChannel();
        epicsWriter = null;
    }

    protected EpicsWriter epicsWriter() { return epicsWriter; }

    protected abstract void write(T value) throws CAException, TimeoutException;

    protected abstract void unbindChannel();


    // The type of CAD inputs is always DBR_STRING. But some systems don't use CAD inputs to set configuration
    // parameters, and there is at least one instrument (I'm looking at you, Flamingos-2) where CAD records were
    // recreated with typed inputs. Therefore, I need two implementations for CaParameter.
    static private final class CaTypedParameter<T> extends CaParameterImpl<T> {
        private ReadWriteClientEpicsChannel<T> rwChannel;

        private CaTypedParameter(String name, String channel, String description,
                                    EpicsWriter epicsWriter) {
            super(name, channel, description, epicsWriter);
        }

        protected void bind(ReadWriteClientEpicsChannel<T> rwChannel) {
            this.rwChannel = rwChannel;
        }

        protected void unbindChannel() {
            try {
                if(rwChannel!=null) {
                    epicsWriter().destroyChannel(rwChannel);
                }
            } catch (CAException e) {
                LOG.warn(e.getMessage());
            }
            rwChannel = null;
        }

        protected void write(T value) throws CAException, TimeoutException {
            if(rwChannel!=null) {
                rwChannel.setValue(value);
            } else {
                LOG.warn("Tried to set value to unbound channel " + channel());
            }
        }
    }

    static private final class CaCADParameter<T> extends CaParameterImpl<T> {
        private ReadWriteClientEpicsChannel<String> rwChannel;

        private CaCADParameter(String name, String channel, String description,
                                            EpicsWriter epicsWriter) {
            super(name, channel, description, epicsWriter);

            try {
                rwChannel = epicsWriter.getStringChannel(channel);
            } catch(Throwable e) {
                LOG.warn(e.getMessage());
            }
        }

        protected void unbindChannel() {
            try {
                if(rwChannel!=null) {
                    epicsWriter().destroyChannel(rwChannel);
                }
            } catch (CAException e) {
                LOG.warn(e.getMessage());
            }
            rwChannel = null;
        }

        protected void write(T value) throws CAException, TimeoutException {
            if(rwChannel!=null) {
                rwChannel.setValue(value.toString());
            } else {
                LOG.warn("Tried to set value to unbound channel " + channel());
            }
        }
    }

    static public CaParameterImpl<Double> createDoubleParameter(
            String name, String channel, String description, EpicsWriter epicsWriter) {
        return createDoubleParameter(name, channel, description, epicsWriter, true);
    }
    static public CaParameterImpl<Double> createDoubleParameter(
            String name, String channel, String description, EpicsWriter epicsWriter, boolean isCADParameter) {
        if(isCADParameter) {
            return new CaCADParameter<>(name, channel, description, epicsWriter);
        } else {
            CaTypedParameter<Double> param = new CaTypedParameter<>(name, channel, description, epicsWriter);
            try {
                param.bind(epicsWriter.getDoubleChannel(channel));
            } catch(Throwable e) {
                LOG.warn(e.getMessage());
            }
            return param;
        }
    }

    static public CaParameterImpl<Integer> createIntegerParameter(
            String name, String channel, String description, EpicsWriter epicsWriter) {
        return createIntegerParameter(name, channel, description, epicsWriter, true);
    }
    static public CaParameterImpl<Integer> createIntegerParameter(
            String name, String channel, String description, EpicsWriter epicsWriter, boolean isCADParameter) {
        if(isCADParameter) {
            return new CaCADParameter<>(name, channel, description, epicsWriter);
        } else {
            CaTypedParameter<Integer> param = new CaTypedParameter<>(name, channel, description, epicsWriter);
            try {
                param.bind(epicsWriter.getIntegerChannel(channel));
            } catch(Throwable e) {
                LOG.warn(e.getMessage());
            }
            return param;
        }
    }

    static public CaParameterImpl<Float> createFloatParameter(
            String name, String channel, String description, EpicsWriter epicsWriter) {
        return createFloatParameter(name, channel, description, epicsWriter, true);
    }
    static public CaParameterImpl<Float> createFloatParameter(
            String name, String channel, String description, EpicsWriter epicsWriter, boolean isCADParameter) {
        if(isCADParameter) {
            return new CaCADParameter<>(name, channel, description, epicsWriter);
        } else {
            CaTypedParameter<Float> param = new CaTypedParameter<>(name, channel, description, epicsWriter);
            try {
                param.bind(epicsWriter.getFloatChannel(channel));
            } catch(Throwable e) {
                LOG.warn(e.getMessage());
            }
            return param;
        }
    }

    static public <E extends Enum<E> > CaParameterImpl<E> createEnumParameter(
            String name, String channel, String description, Class<E> enumType, EpicsWriter epicsWriter) {
        return createEnumParameter(name, channel, description, enumType, epicsWriter, true);
    }
    static public <E extends Enum<E> > CaParameterImpl<E> createEnumParameter(
            String name, String channel, String description, Class<E> enumType, EpicsWriter epicsWriter, boolean isCADParameter) {
        if(isCADParameter) {
            return new CaCADParameter<E>(name, channel, description, epicsWriter);
        } else {
            CaTypedParameter<E> param = new CaTypedParameter<>(name, channel, description, epicsWriter);
            try {
                param.bind(epicsWriter.getEnumChannel(channel, enumType));
            } catch(Throwable e) {
                LOG.warn(e.getMessage());
            }
            return param;
        }
    }

    static public CaParameterImpl<String> createStringParameter(
            String name, String channel, String description, EpicsWriter epicsWriter) {
        return createStringParameter(name, channel, description, epicsWriter, true);
    }
    static public CaParameterImpl<String> createStringParameter(
            String name, String channel, String description, EpicsWriter epicsWriter, boolean isCADParameter) {
        // If the parameter is of type String there is no need to make a difference between CAD and non CAD
        // parameters. But I will keep the differentiation, just in case I need to specialize CaCADParameter even
        // more in the future.
        if(isCADParameter) {
            return new CaCADParameter<>(name, channel, description, epicsWriter);
        } else {
            CaTypedParameter<String> param = new CaTypedParameter<>(name, channel, description, epicsWriter);
            try {
                param.bind(epicsWriter.getStringChannel(channel));
            } catch(Throwable e) {
                LOG.warn(e.getMessage());
            }
            return param;
        }
    }


}
