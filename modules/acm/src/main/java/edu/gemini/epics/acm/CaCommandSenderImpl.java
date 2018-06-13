/*
 * Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import edu.gemini.epics.EpicsService;
import edu.gemini.epics.EpicsWriter;
import edu.gemini.epics.ReadWriteClientEpicsChannel;
import edu.gemini.epics.impl.EpicsWriterImpl;
import gov.aps.jca.CAException;
import gov.aps.jca.TimeoutException;

final class CaCommandSenderImpl implements CaCommandSender {

    private static final Logger LOG = LoggerFactory.getLogger(CaCommandSenderImpl.class.getName());

    private static final String DIR_SUFFIX = ".DIR";
    private final String name;
    private final String description;
    private final Map<String, CaParameterImpl<String>> stringParameters;
    private final Map<String, CaParameterImpl<Double>> doubleParameters;
    private final Map<String, CaParameterImpl<Float>> floatParameters;
    private final Map<String, CaParameterImpl<Integer>> integerParameters;
    private final Map<String, Object> enumParameters;
    private final CaApplySender apply;
    private EpicsWriter epicsWriter;
    private ReadWriteClientEpicsChannel<CadDirective> dirChannel;

    private CaCommandSenderImpl(String name, CaApplySender apply,
                                String description, EpicsService epicsService) {
        this(name, apply, description, epicsService, null);
    }

    public CaCommandSenderImpl(String name, CaApplySender apply,
            String description, EpicsService epicsService, String cadName) {
        this.name = name;
        this.apply = apply;
        this.description = description;
        this.epicsWriter = new EpicsWriterImpl(epicsService);

        if (cadName != null) {
            try {
                dirChannel = epicsWriter.getEnumChannel(cadName + DIR_SUFFIX, CadDirective.class);
            } catch(Throwable e) {
                LOG.warn(e.getMessage());
            }
        }

        stringParameters = new HashMap<>();
        doubleParameters = new HashMap<>();
        floatParameters = new HashMap<>();
        integerParameters = new HashMap<>();
        enumParameters = new HashMap<>();
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Set<String> getInfo() {
        Set<String> set = new HashSet<>(doubleParameters.keySet());
        set.addAll(floatParameters.keySet());
        set.addAll(integerParameters.keySet());
        set.addAll(stringParameters.keySet());
        set.addAll(enumParameters.keySet());
        return set;
    }

    @Override
    public CaApplySender getApplySender() { return apply; }

    public void unbind() {
        assert (epicsWriter != null);

        for (CaParameterImpl<?> param : stringParameters.values()) {
            param.unbind();
        }
        for (CaParameterImpl<?> param : doubleParameters.values()) {
            param.unbind();
        }
        for (CaParameterImpl<?> param : floatParameters.values()) {
            param.unbind();
        }
        for (CaParameterImpl<?> param : integerParameters.values()) {
            param.unbind();
        }

        stringParameters.clear();
        doubleParameters.clear();
        floatParameters.clear();
        integerParameters.clear();
        enumParameters.clear();

        try {
            if (dirChannel != null) {
                epicsWriter.destroyChannel(dirChannel);
                dirChannel = null;
            }
        } catch (CAException e) {
            LOG.warn(e.getMessage());
        }

        epicsWriter = null;
    }

    @Override
    public CaCommandMonitor post() {
        return apply.post();
    }

    @Override
    public CaCommandMonitor postWait() throws InterruptedException {
        return apply.postWait();
    }

    @Override
    public CaCommandMonitor postCallback(CaCommandListener callback) {
        return apply.postCallback(callback);
    }

    @Override
    public CaParameter<Integer> addInteger(String name, String channel)
            throws CaException {
        return addInteger(name, channel, null, true);
    }

    @Override
    public CaParameter<Integer> addInteger(String name, String channel, boolean isCADParameter) throws CaException {
        return addInteger(name, channel, null, isCADParameter);
    }

    @Override
    public CaParameter<Integer> addInteger(String name, String channel,
            String description) throws CaException {
        return addInteger(name, channel, description, true);
    }

    @Override
    public CaParameter<Integer> addInteger(String name, String channel, String description, boolean isCADParameter) throws CaException {
        CaParameterImpl<Integer> param = integerParameters.get(name);
        if (param == null) {
            if (alreadyExist(name)) {
                throw new CaException(
                        "Parameter already exists with a different type.");
            } else {
                param = CaParameterImpl.createIntegerParameter(name, channel,
                        description, epicsWriter, isCADParameter);
                integerParameters.put(name, param);
            }
        } else {
            if (!channel.equals(param.channel())) {
                throw new CaException(
                        "Parameter already exists for a different channel.");
            }
        }
        return param;
    }

    @Override
    public CaParameter<Double> addDouble(String name, String channel)
            throws CaException {
        return addDouble(name, channel, null, true);
    }

    @Override
    public CaParameter<Double> addDouble(String name, String channel, boolean isCADParameter) throws CaException {
        return addDouble(name, channel, null, isCADParameter);
    }

    @Override
    public CaParameter<Double> addDouble(String name, String channel,
            String description) throws CaException {
        return addDouble(name, channel, description, true);
    }

    @Override
    public CaParameter<Double> addDouble(String name, String channel, String description, boolean isCADParameter) throws CaException {
        CaParameterImpl<Double> param = doubleParameters.get(name);
        if (param == null) {
            if (alreadyExist(name)) {
                throw new CaException(
                        "Parameter already exists with a different type.");
            } else {
                param = CaParameterImpl.createDoubleParameter(name, channel,
                        description, epicsWriter, isCADParameter);
                doubleParameters.put(name, param);
            }
        } else {
            if (!channel.equals(param.channel())) {
                throw new CaException(
                        "Parameter already exists for a different channel.");
            }
        }
        return param;
    }

    @Override
    public <T extends Enum<T>> CaParameter<T> addEnum(String name, String channel, Class<T> enumType, String description) throws CaException {
        return addEnum(name, channel, enumType, CaParameterImpl.class, description, true);
    }

    @Override
    public <T extends Enum<T>> CaParameter<T> addEnum(String name, String channel, Class<T> enumType) throws CaException {
        return addEnum(name, channel, enumType, CaParameterImpl.class, null, true);
    }

    @Override
    public <T extends Enum<T>> CaParameter<T> addEnum(String name, String channel, Class<T> enumType, String description, boolean isCADParameter) throws CaException {
        return addEnum(name, channel, enumType, CaParameterImpl.class, description, isCADParameter);
    }

    private <T extends Enum<T>, A extends CaParameterImpl<T> > CaParameter<T> addEnum(String name, String channel, Class<T> enumType, Class<A> paramType, String description, boolean isCADParameter) throws CaException {
        CaParameterImpl<T> param;
        try {
            param = paramType.cast(enumParameters.get(name));
        } catch(ClassCastException e) {
            param = null;
        }
        if (param == null) {
            if (alreadyExist(name)) {
                throw new CaException(
                        "Parameter already exists with a different type.");
            } else {
                param = CaParameterImpl.createEnumParameter(name, channel, description, enumType,
                        epicsWriter, isCADParameter);
                enumParameters.put(name, param);
            }
        } else {
            if (!channel.equals(param.channel())) {
                throw new CaException(
                        "Parameter already exists for a different channel.");
            }
        }
        return param;
    }

    @Override
    public <T extends Enum<T>> CaParameter<T> addEnum(String name, String channel, Class<T> enumType, boolean isCADParameter) throws CaException {
        return addEnum(name, channel, enumType, CaParameterImpl.class, null, isCADParameter);
    }

    @Override
    public CaParameter<Float> addFloat(String name, String channel)
            throws CaException {
        return addFloat(name, channel, null, true);
    }

    @Override
    public CaParameter<Float> addFloat(String name, String channel, boolean isCADParameter) throws CaException {
        return addFloat(name, channel, null, isCADParameter);
    }

    @Override
    public CaParameter<Float> addFloat(String name, String channel,
            String description) throws CaException {
        return addFloat(name, channel, description, true);
    }

    @Override
    public CaParameter<Float> addFloat(String name, String channel, String description, boolean isCADParameter) throws CaException {
        CaParameterImpl<Float> param = floatParameters.get(name);
        if (param == null) {
            if (alreadyExist(name)) {
                throw new CaException(
                        "Parameter already exists with a different type.");
            } else {
                param = CaParameterImpl.createFloatParameter(name, channel,
                        description, epicsWriter, isCADParameter);
                floatParameters.put(name, param);
            }
        } else {
            if (!channel.equals(param.channel())) {
                throw new CaException(
                        "Parameter already exists for a different channel.");
            }
        }
        return param;
    }

    @Override
    public CaParameter<String> addString(String name, String channel)
            throws CaException {
        return addString(name, channel, null, true);
    }

    @Override
    public CaParameter<String> addString(String name, String channel, boolean isCADParameter) throws CaException {
        return addString(name, channel, null, isCADParameter);
    }

    @Override
    public CaParameter<String> addString(String name, String channel,
            String description) throws CaException {
        return addString(name, channel, description, true);
    }

    @Override
    public CaParameter<String> addString(String name, String channel, String description, boolean isCADParameter) throws CaException {
        CaParameterImpl<String> param = stringParameters.get(name);
        if (param == null) {
            if (alreadyExist(name)) {
                throw new CaException(
                        "Parameter already exists with a different type.");
            } else {
                param = CaParameterImpl.createStringParameter(name, channel,
                        description, epicsWriter);
                stringParameters.put(name, param);
            }
        } else {
            if (!channel.equals(param.channel())) {
                throw new CaException(
                        "Parameter already exists for a different channel.");
            }
        }
        return param;
    }

    @Override
    public CaParameter<Integer> getInteger(String name) {
        return integerParameters.get(name);
    }

    @Override
    public CaParameter<Double> getDouble(String name) {
        return doubleParameters.get(name);
    }

    @Override
    public CaParameter<Float> getFloat(String name) {
        return floatParameters.get(name);
    }

    @Override
    public CaParameter<String> getString(String name) {
        return stringParameters.get(name);
    }

    @Override
    public void remove(String name) {
        doubleParameters.remove(name);
        floatParameters.remove(name);
        integerParameters.remove(name);
        stringParameters.remove(name);
        enumParameters.remove(name);
    }

    private boolean alreadyExist(String name) {
        return doubleParameters.containsKey(name)
                || floatParameters.containsKey(name)
                || integerParameters.containsKey(name)
                || stringParameters.containsKey(name)
                || enumParameters.containsKey(name);
    }

    @Override
    public void mark() throws TimeoutException {
        if (dirChannel != null) {
            try {
                dirChannel.setValue(CadDirective.MARK);
            } catch (CAException e) {
                LOG.warn(e.getMessage());
            }
        }
    }

    @Override
    public void clear() throws TimeoutException {
        if (dirChannel != null) {
            try {
                dirChannel.setValue(CadDirective.CLEAR);
            } catch (CAException e) {
                LOG.warn(e.getMessage());
            }
        }
    }

    @Override
    public String getDescription() {
        return description;
    }


}
