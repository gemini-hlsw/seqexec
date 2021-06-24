/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import edu.gemini.epics.EpicsReader;
import edu.gemini.epics.EpicsWriter;
import edu.gemini.epics.ReadOnlyClientEpicsChannel;
import edu.gemini.epics.ReadWriteClientEpicsChannel;
import edu.gemini.epics.api.ChannelListener;
import gov.aps.jca.CAException;
import gov.aps.jca.TimeoutException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


final class CaApplyRecord {

    private static final Logger LOG = LoggerFactory.getLogger(CaApplyRecord.class
            .getName());

    private static final String DIR_SUFFIX = ".DIR";
    private static final String VAL_SUFFIX = ".VAL";
    private static final String MSG_SUFFIX = ".MESS";

    private final String epicsName;

    private final EpicsReader epicsReader;
    private final EpicsWriter epicsWriter;
    private ReadWriteClientEpicsChannel<CadDirective> dir;
    private ReadOnlyClientEpicsChannel<Integer> val;
    private ReadOnlyClientEpicsChannel<String> mess;

    private ChannelListener<Integer> valListener;

    CaApplyRecord(String epicsName, EpicsReader epicsReader, EpicsWriter epicsWriter) {
        this.epicsName = epicsName;
        assert(epicsReader != null);
        assert(epicsWriter != null);

        this.epicsReader = epicsReader;
        this.epicsWriter = epicsWriter;

        updateChannels();
    }

    synchronized void updateChannels() {
        if (dir == null) {
            try {
                dir = epicsWriter.getEnumChannel(epicsName + DIR_SUFFIX, CadDirective.class);
            } catch (Throwable e) {
                LOG.warn(e.getMessage());
            }
        }
        if (val == null) {
            try {
                val = epicsReader.getIntegerChannel(epicsName + VAL_SUFFIX);
                if (valListener != null) {
                    val.registerListener(valListener);
                }
            } catch (Throwable e) {
                LOG.warn(e.getMessage());
            }
        }
        if (mess == null) {
            try {
                mess = epicsReader.getStringChannel(epicsName + MSG_SUFFIX);
            } catch (Throwable e) {
                LOG.warn(e.getMessage());
            }
        }
    }

    synchronized void unbind() {
        if(dir!=null) {
            try {
                epicsWriter.destroyChannel(dir);
            } catch (CAException e) {
                LOG.warn(e.getMessage());
            }
            dir = null;
        }

        if(val!=null) {
            try {
                epicsReader.destroyChannel(val);
            } catch (CAException e) {
                LOG.warn(e.getMessage());
            }
            val = null;
        }

        if(mess!=null) {
            try {
                epicsReader.destroyChannel(mess);
            } catch (CAException e) {
                LOG.warn(e.getMessage());
            }
            mess = null;
        }
    }

    synchronized void registerValListener(ChannelListener<Integer> listener) throws CAException {
        if(val!=null) {
            val.registerListener(listener);
        }
        valListener = listener;
    }

    synchronized void unregisterValListener(ChannelListener<Integer> listener) throws CAException {
        if(val!=null) {
            val.unRegisterListener(listener);
        }
        valListener = null;
    }

    synchronized int getValValue() throws CAException, TimeoutException {
        if(val!=null) {
            return val.getFirst();
        } else {
            throw new CAException("Tried to read from unbound channel  " + epicsName + VAL_SUFFIX);
        }
    }

    synchronized String getMessValue() throws CAException, TimeoutException {
        if(mess!=null) {
            return mess.getFirst();
        } else {
            throw new CAException("Tried to read from unbound channel  " + epicsName + MSG_SUFFIX);
        }
    }

    synchronized void setDir(CadDirective directive) throws CAException, TimeoutException {
        if(dir!=null) {
            dir.setValue(directive);
        } else {
            throw new CAException("Tried to write to unbound channel  " + epicsName + DIR_SUFFIX);
        }
    }

    public String getEpicsName() {
        return epicsName;
    }
}
