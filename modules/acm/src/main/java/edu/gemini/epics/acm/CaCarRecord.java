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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

final class CaCarRecord<C extends Enum<C> & CarStateGeneric> {
    private static final Logger LOG = LoggerFactory.getLogger(CaCarRecord.class
            .getName());

    private static final String CAR_VAL_SUFFIX = ".VAL";
    private static final String CAR_CLID_SUFFIX = ".CLID";
    private static final String CAR_OMSS_SUFFIX = ".OMSS";

    private final String epicsName;
    private EpicsReader epicsReader;
    private Class<C> carClass;
    private ReadOnlyClientEpicsChannel<Integer> clid;
    private ReadOnlyClientEpicsChannel<C> val;
    private ReadOnlyClientEpicsChannel<String> omss;

    private ChannelListener<Integer> clidListener;
    private ChannelListener<C> valListener;

    CaCarRecord(String epicsName, Class<C> carClass, EpicsService epicsService) {
        this.epicsName = epicsName;
        this.carClass = carClass;
        epicsReader = new EpicsReaderImpl(epicsService);

        updateChannels();
    }

    synchronized void updateChannels() {
        try {
            clid = epicsReader.getIntegerChannel(epicsName + CAR_CLID_SUFFIX);
            if(clidListener!=null) {
                clid.registerListener(clidListener);
            }
        } catch(Throwable e) {
            LOG.warn(e.getMessage());
        }
        try {
            val = epicsReader.getEnumChannel(epicsName + CAR_VAL_SUFFIX, carClass);
            if(valListener!=null) {
                val.registerListener(valListener);
            }
        } catch(Throwable e) {
            LOG.warn(e.getMessage());
        }
        try {
            omss = epicsReader.getStringChannel(epicsName + CAR_OMSS_SUFFIX);
        } catch(Throwable e) {
            LOG.warn(e.getMessage());
        }
    }

    synchronized void unbind() {
        try {
            if(clid!=null) {
                epicsReader.destroyChannel(clid);
            }
        } catch (CAException e) {
            LOG.warn(e.getMessage());
        }
        clid = null;

        try {
            if(val!=null){
                epicsReader.destroyChannel(val);
            }
        } catch (CAException e) {
            LOG.warn(e.getMessage());
        }
        val = null;

        try {
            if(omss!=null) {
                epicsReader.destroyChannel(omss);
            }
        } catch (CAException e) {
            LOG.warn(e.getMessage());
        }
        omss = null;

        epicsReader = null;
    }

    String getEpicsName() {
        return epicsName;
    }

    void registerClidListener(ChannelListener<Integer> listener) throws CAException {
        if (clid!=null) {
            clid.registerListener(listener);
        }
        clidListener = listener;
    }

    void unregisterClidListener(ChannelListener<Integer> listener) throws CAException {
        if (clid!=null) {
            clid.unRegisterListener(listener);
        }
        clidListener = null;
    }

    void registerValListener(ChannelListener<C> listener) throws CAException {
        if (val!=null) {
            val.registerListener(listener);
        }
        valListener = listener;
    }

    void unregisterValListener(ChannelListener<C> listener) throws CAException {
        if (val!=null) {
            val.unRegisterListener(listener);
        }
        valListener = null;
    }

    C getValValue() throws CAException, TimeoutException {
        return val.getFirst();
    }

    String getOmssValue() throws CAException, TimeoutException {
        return omss.getFirst();
    }

}
