/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import com.google.common.collect.ImmutableSet;

import edu.gemini.epics.EpicsService;
import edu.gemini.epics.EpicsReader;
import edu.gemini.epics.impl.EpicsReaderImpl;
import edu.gemini.epics.EpicsWriter;
import edu.gemini.epics.impl.EpicsWriterImpl;
import gov.aps.jca.CAException;

/**
 * Works as the main access point for the EPICS Action Command Model API. It
 * acts as a factory for all the API objects, and controls the life cycle of the
 * underlying EPICS services.
 * <p>
 * CaService is a singleton.
 * <p>
 * The list of IP addresses used to discover EPICS channels can be set by
 * calling <code>setAddressList()</code> before the first call to getInstance().
 * Calling <code>setAddressList()</code> after the singleton has been created
 * has no effect. If no list is set when the singleton is created, it will use
 * the value of the environment variable EPICS_CA_ADDR_LIST.
 * <p>
 * The method <code>unbind()</code> must be called to stop the service. Failing
 * to do it will keep the application running. References to the CaService must
 * not be used once <code>unbind()</code> has been called.
 *
 * @author jluhrs
 *
 */
public final class CaService {

    private static final String EPICS_CA_ADDR_LIST = "EPICS_CA_ADDR_LIST";
    private EpicsService epicsService;
    private final Map<String, StatusAcceptorWithResource> statusAcceptors;
    private final Map<String, ApplySenderWithResource> applySenders;
    private final Map<String, ApplySenderWithResource> observeSenders;
    private final Map<String, ApplySenderWithResource> continuousCmdSenders;
    private final Map<String, CommandSenderWithResource> commandSenders;
    private final Map<String, TaskControlWithResource> taskControlSenders;
    static private String addrList = "";
    static private Duration ioTimeout = Duration.ofSeconds(1);
    static private CaService theInstance;
    static private Lock instanceLock = new ReentrantLock();

    private CaService(String addrList, Duration timeout) {
        statusAcceptors = new HashMap<>();
        applySenders = new HashMap<>();
        observeSenders = new HashMap<>();
        continuousCmdSenders = new HashMap<>();
        commandSenders = new HashMap<>();
        taskControlSenders = new HashMap<>();
        epicsService = new EpicsService(addrList, Double.valueOf(timeout.getSeconds()));

        epicsService.startService();
    }

    private CaService() {

        this(System.getenv(EPICS_CA_ADDR_LIST).replaceAll("\\\\ ", " "), Duration.ofSeconds(1));

    }

    /**
     * Sets the list of IP addresses used to discover EPICS channels.
     *
     * @param addrList
     *            the list of IP addresses used to discover EPICS channels.
     */
    public static void setAddressList(String addrList) {
        CaService.addrList = addrList;
    }

    /**
     * Sets the timeout to wait for EPICS IO requests.
     *
     * @param timeout
     *            time to wait for EPICS requests.
     */
    public static void setIOTimeout(Duration timeout) {
        CaService.ioTimeout = timeout;
    }

    /**
     * Retrieves the CaService single instance.
     *
     * @return the single instance of CaService.
     */
    public static CaService getInstance() {
        // Double check avoids using the lock after the instance was created.
        if (theInstance == null) {
            instanceLock.lock();
            if (theInstance == null) {
                if (addrList == null) {
                    theInstance = new CaService();
                } else {
                    theInstance = new CaService(addrList, ioTimeout);
                }
            }
            instanceLock.unlock();
        }
        return theInstance;
    }

    /**
     * Free resources and stop the underlying EPICS service.
     */
    public void unbind() {
        assert (epicsService != null);

        statusAcceptors.values().forEach(CaResource::unbind);

        applySenders.values().forEach(CaResource::unbind);

        observeSenders.values().forEach(CaResource::unbind);

        continuousCmdSenders.values().forEach(CaResource::unbind);

        commandSenders.values().forEach(CaResource::unbind);

        taskControlSenders.values().forEach(CaResource::unbind);

        epicsService.stopService();
        epicsService = null;
        theInstance = null;
    }

    /**
     * Creates a status acceptor. If the status acceptor already exists, it
     * returns the existing object.
     *
     * @param name
     *            the name of the new status acceptor.
     * @param description
     *            optional description for the status acceptor
     * @return the status acceptor.
     */
    public CaStatusAcceptor createStatusAcceptor(String name, String description) {
        CaStatusAcceptor a = statusAcceptors.get(name);
        if (a == null) {
            StatusAcceptorWithResource b = new CaStatusAcceptorImpl(name, description, epicsService);
            statusAcceptors.put(name, b);
            return b;
        } else {
            return a;
        }
    }

    public CaStatusAcceptor createStatusAcceptor(String name) {
        return createStatusAcceptor(name, null);
    }

    /**
     * Retrieves an existing status acceptor.
     *
     * @param name
     *            the name of the status acceptor.
     * @return the status acceptor, or <code>null</code> if it does not exist.
     */
    public CaStatusAcceptor getStatusAcceptor(String name) {
        return statusAcceptors.get(name);
    }

    /**
     * Creates an apply sender. If the apply sender already exists, it returns
     * the existing object.
     *
     * @param name
     *            the name of the new apply sender.
     * @param applyRecord
     *            the name of the EPICS apply record.
     * @param carRecord
     *            the name of the EPICS CAR record associated with the apply.
     * @param description
     *            optional description for the apply sender.
     * @return the apply sender.
     * @throws CAException
     *            Error in the Channel Access library.
     */
    public CaApplySender createApplySender(String name, String applyRecord,
            String carRecord, Boolean gem5, String description) throws CAException {
        CaApplySender a = applySenders.get(name);
        if (a == null) {
            ApplySenderWithResource b;
            EpicsReader epicsReader = new EpicsReaderImpl(epicsService);
            EpicsWriter epicsWriter = new EpicsWriterImpl(epicsService);
            if(gem5) {
                b = new CaApplySenderImpl<>(name, applyRecord, carRecord,
                        description, CarStateGEM5.class, epicsReader, epicsWriter);
            }
            else {
                b = new CaApplySenderImpl<>(name, applyRecord, carRecord,
                        description, CarState.class, epicsReader, epicsWriter);
            }
            applySenders.put(name, b);
            return b;
        } else {
            return a;
        }
    }

    public CaApplySender createApplySender(String name, String applyRecord,
            String carRecord, Boolean gem5) throws CAException {
        return createApplySender(name, applyRecord, carRecord, gem5, null);
    }

    /**
     * Creates an apply sender specialized for Observe commands. If the apply
     * sender already exists, it returns the existing object.
     *
     * @param name
     *            the name of the new apply sender.
     * @param applyRecord
     *            the name of the EPICS apply record.
     * @param carRecord
     *            the name of the EPICS CAR record associated with the apply.
     * @param observeCarRecord
     *            the name of the EPICS CAR record for the observe state.
     * @param gem5
     *            CAR record uses GEM5 definition.
     * @param stopCmdRecord
     *            the name of the EPICS CAD for the stop command.
     * @param abortCmdRecord
     *            the name of the EPICS CAD for the abort command.
     * @param description
     *            optional description for the apply sender.
     * @return the apply sender.
     * @throws CAException
     *            Error in the Channel Access library.
     */
    public CaApplySender createObserveSender(String name, String applyRecord,
            String carRecord, String observeCarRecord, Boolean gem5, String stopCmdRecord, String abortCmdRecord, String description) throws CAException {
        CaApplySender a = observeSenders.get(name);
        if (a == null) {
            ApplySenderWithResource b;
            EpicsReader epicsReader = new EpicsReaderImpl(epicsService);
            EpicsWriter epicsWriter = new EpicsWriterImpl(epicsService);
            if(gem5) {
                b = new CaObserveSenderImpl<CarStateGEM5>(name, applyRecord, carRecord, observeCarRecord, stopCmdRecord, abortCmdRecord,
                        description, CarStateGEM5.class, epicsReader, epicsWriter);
            }
            else  {
                b = new CaObserveSenderImpl<CarState>(name, applyRecord, carRecord, observeCarRecord, stopCmdRecord, abortCmdRecord,
                        description, CarState.class, epicsReader, epicsWriter);
            }
            observeSenders.put(name, b);
            return b;
        } else {
            return a;
        }
    }

    public CaApplySender createObserveSender(String name, String applyRecord,
            String carRecord, String observeCarRecord, Boolean gem5) throws CAException {
        return createObserveSender(name, applyRecord, carRecord, observeCarRecord, gem5, null, null, null);
    }

    /**
     * Creates an apply sender specialized for Observe commands. If the apply sender already exists, it returns the
     * existing object.
     *
     * @param name
     *            the name of the new apply sender.
     * @param applyRecord
     *            the name of the EPICS apply record.
     * @param carRecord
     *            the name of the EPICS observe CAR record.
     * @param gem5
     *            CAR record uses GEM5 definition.
     * @param stopCmdRecord
     *            the name of the EPICS CAD for the stop command.
     * @param abortCmdRecord
     *            the name of the EPICS CAD for the abort command.
     * @param description
     *            optional description for the apply sender.
     * @return the apply sender.
     * @throws CAException
     *            Error in the Channel Access library.
     */
    public CaApplySender createObserveSender(String name, String applyRecord,
                                             String carRecord, Boolean gem5, String stopCmdRecord, String abortCmdRecord, String description) throws CAException {
        CaApplySender a = observeSenders.get(name);
        if (a == null) {
            ApplySenderWithResource b;
            EpicsReader epicsReader = new EpicsReaderImpl(epicsService);
            EpicsWriter epicsWriter = new EpicsWriterImpl(epicsService);
            if(gem5) {
                b = new CaSimpleObserveSenderImpl<CarStateGEM5>(name, applyRecord, carRecord, stopCmdRecord, abortCmdRecord,
                        description, CarStateGEM5.class, epicsReader, epicsWriter);
            }
            else  {
                b = new CaSimpleObserveSenderImpl<CarState>(name, applyRecord, carRecord, stopCmdRecord, abortCmdRecord,
                        description, CarState.class, epicsReader, epicsWriter);
            }
            observeSenders.put(name, b);
            return b;
        } else {
            return a;
        }
    }

    /**
     * Retrieves an existing apply sender.
     *
     * @param name
     *            the name of the apply sender.
     * @return the apply sender, or <code>null</code> if it does not exist.
     */
    public CaApplySender getApplySender(String name) {
        return applySenders.get(name);
    }

    /**
     * Creates an apply sender for continuous commands (commands that are started and stay busy until another command
     * stops them). If the apply sender already exists, it returns the existing object.
     *
     * @param name
     *            the name of the new apply sender.
     * @param applyRecord
     *            the name of the EPICS apply record.
     * @param carRecord
     *            the name of the EPICS observe CAR record.
     * @param gem5
     *            CAR record uses GEM5 definition.
     * @param description
     *            optional description for the apply sender.
     * @return the apply sender.
     * @throws CAException
     *            Error in the Channel Access library.
     */
    public CaApplySender createContinuousCommandSender(String name, String applyRecord,
                                             String carRecord, Boolean gem5, String description) throws CAException {
        CaApplySender a = continuousCmdSenders.get(name);
        if (a == null) {
            ApplySenderWithResource b;
            EpicsReader epicsReader = new EpicsReaderImpl(epicsService);
            EpicsWriter epicsWriter = new EpicsWriterImpl(epicsService);
            if(gem5) {
                b = new CaContinuousApplySenderImpl<CarStateGEM5>(name, applyRecord, carRecord, description,
                        CarStateGEM5.class, epicsReader, epicsWriter);
            }
            else  {
                b = new CaContinuousApplySenderImpl<CarState>(name, applyRecord, carRecord, description,
                        CarState.class, epicsReader, epicsWriter);
            }
            continuousCmdSenders.put(name, b);
            return b;
        } else {
            return a;
        }
    }

    /**
     * Retrieves an existing apply sender.
     *
     * @param name
     *            the name of the apply sender.
     * @return the apply sender, or <code>null</code> if it does not exist.
     */
    public CaApplySender getContinuousCommandSender(String name) {
        return applySenders.get(name);
    }

    /**
     * Creates an command sender. If the command sender already exists, it
     * returns the existing object.
     *
     * @param name
     *            the name of the new command sender.
     * @param apply
     *            the apply sender used to trigger the command.
     * @param cadName
     *            the EPICS name of the CAD record, needed to mark the record
     *            for execution if it has no parameters.
     * @param description
     *            optional description for the command sender
     * @return the command sender.
     */
    public CaCommandSender createCommandSender(String name,
                                               CaApplySender apply, String cadName, String description) {
        CaCommandSender a = commandSenders.get(name);
        if (a == null) {
            CommandSenderWithResource b = new CaCommandSenderImpl(name, apply, description,
                    epicsService, cadName);
            commandSenders.put(name, b);
            return b;
        } else {
            return a;
        }
    }

    public CaCommandSender createCommandSender(String name,
            CaApplySender apply, String cadName) {
        return createCommandSender(name, apply, cadName, null);
    }

    /**
     * Retrieves an existing command sender.
     *
     * @param name
     *            the name of the command sender.
     * @return the command sender, or <code>null</code> if it does not exist.
     */
    public CaCommandSender getCommandSender(String name) {
        return commandSenders.get(name);
    }

    /**
     * Retrieves the names of all existing command senders
     *
     * @return a set of all the command sender names.
     */
    public ImmutableSet<String> getCommandSenderNames() {
        return ImmutableSet.copyOf(commandSenders.keySet());
    }

    /**
     * Retrieves the names of all existing apply senders
     *
     * @return a set of all the command sender names.
     */
    public ImmutableSet<String> getApplySenderNames() {
        return ImmutableSet.copyOf(applySenders.keySet());
    }

    /**
     * Retrieves the names of all existing status acceptors
     *
     * @return a set of all the status acceptors names.
     */
    public ImmutableSet<String> getStatusAcceptorsNames() {
        return ImmutableSet.copyOf(statusAcceptors.keySet());
    }

    /**
     * Destroys a command sender with a given name. If the command sender does
     * not exists, it does nothing.
     *
     * @param name the name of the command sender to destroy.
     */
    public void destroyCommandSender(String name) {
        CaResource cs = commandSenders.remove(name);
        if (cs != null) {
            cs.unbind();
        }
    }

    /**
     * Destroys a apply sender with a given name. If the apply sender does
     * not exists, it does nothing.
     *
     * @param name the name of the apply sender to destroy.
     */
    public void destroyApplySender(String name) {
        CaResource apply = applySenders.remove(name);
        if (apply != null) {
            apply.unbind();
        }
    }

    /**
     * Destroys a status acceptor with a given name. If the status acceptor does
     * not exists, it does nothing.
     *
     * @param name the name of the status acceptor to destroy.
     */
    public void destroyStatusAcceptor(String name) {
        CaResource sa = statusAcceptors.remove(name);
        if (sa != null) {
            sa.unbind();
        }
    }

    /**
     * Creates a handler for a taskControl record.
     * @param name the name of the TaskControlSender
     * @param recordName the name of the EPICS record
     * @param description the description of the record.
     * @return the TaskControlSender
     * @throws CAException
     *            Error in the Channel Access library.
     */
    public CaTaskControl createTaskControlSender(String name, String recordName, String description)
            throws CAException {
        CaTaskControl a = taskControlSenders.get(name);
        if(a == null) {
            TaskControlWithResource b = new CaTaskControlImpl(name, recordName, description, epicsService);
            taskControlSenders.put(name, b);
            return b;
        } else {
            return a;
        }
    }

    /**
     * Retrieves an existing <code>TaskControlSender</code>, or <code>null</code> if none exists.
     * @param name the name of the <code>TaskControlSender</code>
     * @return the <code>TaskControlSender</code>
     */
    public CaTaskControl getTaskControlSender(String name) {
        return taskControlSenders.get(name);
    }

    /**
     * Destroys an existing <code>TaskControlSender</code>
     * @param name the name of the <code>TaskControlSender</code>
     */
    public void destroyTaskControlSender(String name) {
        CaResource t = taskControlSenders.remove(name);
        if(t != null) {
            t.unbind();
        }
    }

}
