/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
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

interface CommandSenderWithResource extends CaCommandSender, CaResource {}

final class CaCommandSenderImpl implements CommandSenderWithResource {

    private static final Logger LOG = LoggerFactory.getLogger(CaCommandSenderImpl.class.getName());

    private static final String DIR_SUFFIX = ".DIR";
    private final String name;
    private final String description;
    private final CaParameterStorage storage;
    private final CaApplySender apply;
    private EpicsWriter epicsWriter;
    private ReadWriteClientEpicsChannel<CadDirective> dirChannel;

    public CaCommandSenderImpl(String name, CaApplySender apply,
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

        storage = new CaParameterStorage(epicsWriter);
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Set<String> getInfo() {
        return storage.getInfo();
    }

    @Override
    public CaApplySender getApplySender() { return apply; }

    @Override
    public void unbind() {
        assert (epicsWriter != null);

        storage.unbind();

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
    public CaParameter<Integer> addInteger(String name, String channel, String description, boolean isCADParameter)
            throws CaException {
        return storage.addInteger(name, channel, description, isCADParameter);
    }

    @Override
    public CaParameter<Double> addDouble(String name, String channel, String description, boolean isCADParameter)
            throws CaException {
        return storage.addDouble(name, channel, description, isCADParameter);
    }

    @Override
    public <T extends Enum<T>> CaParameter<T> addEnum(String name, String channel, Class<T> enumType,
                                                      String description, boolean isCADParameter)
            throws CaException {
        return storage.addEnum(name, channel, enumType, description, isCADParameter);
    }


    @Override
    public CaParameter<Float> addFloat(String name, String channel, String description, boolean isCADParameter)
            throws CaException {
        return storage.addFloat(name, channel, description, isCADParameter);
    }

   @Override
    public CaParameter<String> addString(String name, String channel, String description, boolean isCADParameter)
           throws CaException {
        return storage.addString(name, channel, description, isCADParameter);
    }

    @Override
    public CaParameter<Integer> getInteger(String name) {
        return storage.getInteger(name);
    }

    @Override
    public CaParameter<Double> getDouble(String name) {
        return storage.getDouble(name);
    }

    @Override
    public CaParameter<Float> getFloat(String name) {
        return storage.getFloat(name);
    }

    @Override
    public CaParameter<String> getString(String name) {
        return storage.getString(name);
    }

    @Override
    public void remove(String name) {
        storage.remove(name);
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
