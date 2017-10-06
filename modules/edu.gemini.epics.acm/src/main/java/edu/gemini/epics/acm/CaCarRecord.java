/*
 * Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
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

import java.util.logging.Logger;

final class CaCarRecord {
    private static final Logger LOG = Logger.getLogger(CaCarRecord.class
            .getName());

    private static final String CAR_VAL_SUFFIX = ".VAL";
    private static final String CAR_CLID_SUFFIX = ".CLID";
    private static final String CAR_OMSS_SUFFIX = ".OMSS";

    private final String epicsName;
    private EpicsReader epicsReader;
    private ReadOnlyClientEpicsChannel<Integer> clid;
    private ReadOnlyClientEpicsChannel<CarState> val;
    private ReadOnlyClientEpicsChannel<String> omss;

    private ChannelListener<Integer> clidListener;
    private ChannelListener<CarState> valListener;

    CaCarRecord(String epicsName, EpicsService epicsService) {
        this.epicsName = epicsName;
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
            LOG.warning(e.getMessage());
        }
        try {
            val = epicsReader.getEnumChannel(epicsName + CAR_VAL_SUFFIX, CarState.class);
            if(valListener!=null) {
                val.registerListener(valListener);
            }
        } catch(Throwable e) {
            LOG.warning(e.getMessage());
        }
        try {
            omss = epicsReader.getStringChannel(epicsName + CAR_OMSS_SUFFIX);
        } catch(Throwable e) {
            LOG.warning(e.getMessage());
        }
    }

    synchronized void unbind() {
        try {
            if(clid!=null) {
                epicsReader.destroyChannel(clid);
            }
        } catch (CAException e) {
            LOG.warning(e.getMessage());
        }
        clid = null;

        try {
            if(val!=null){
                epicsReader.destroyChannel(val);
            }
        } catch (CAException e) {
            LOG.warning(e.getMessage());
        }
        val = null;

        try {
            if(omss!=null) {
                epicsReader.destroyChannel(omss);
            }
        } catch (CAException e) {
            LOG.warning(e.getMessage());
        }
        omss = null;

        epicsReader = null;
    }

    String getEpicsName() {
        return epicsName;
    }

    void registerClidListener(ChannelListener<Integer> listener) throws CAException {
        if(clid!=null) {
            clid.registerListener(listener);
        }
        clidListener = listener;
    }

    void unregisterClidListener(ChannelListener<Integer> listener) throws CAException {
        if(clid!=null) {
            clid.unRegisterListener(listener);
        }
        clidListener = null;
    }

    void registerValListener(ChannelListener<CarState> listener) throws CAException {
        if(val!=null) {
            val.registerListener(listener);
        }
        valListener = listener;
    }

    void unregisterValListener(ChannelListener<CarState> listener) throws CAException {
        if(val!=null) {
            val.unRegisterListener(listener);
        }
        valListener = null;
    }

    CarState getValValue() throws CAException, TimeoutException {
        return val.getFirst();
    }

    String getOmssValue() throws CAException, TimeoutException {
        return omss.getFirst();
    }

}
