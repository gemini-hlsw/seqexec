/*
 * Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import gov.aps.jca.TimeoutException;

import java.util.concurrent.TimeUnit;

/**
 * Defines the interface for Apply Senders. Command Senders require an object
 * that implements this interface to trigger the EPICS commands.
 * 
 * @author jluhrs
 *
 */

public interface CaApplySender {
    /**
     * Retrieves the name of this Apply Sender.
     * 
     * @return the name of this Apply Sender.
     */
    String getName();

    /**
     * Retrieves the name of the EPICS apply record associated with this apply
     * sender.
     * 
     * @return the name of the EPICS apply record.
     */
    String getApply();

    /**
     * Retrieves the name of the EPICS CAR record associated with this apply
     * sender.
     * 
     * @return the name of the EPICS CAR record.
     */
    String getCAR();

    /**
     * Retrieves the description for this Apply Sender
     * 
     * @return the description of this Apply Sender
     */
    String getDescription();

    /**
     * Set the command execution timeout. If a command execution takes longer
     * than this time, it will be declared on error.
     * 
     * @param timeout
     *            the time interval
     * @param timeUnit
     *            the time units for the time interval
     */
    void setTimeout(long timeout, TimeUnit timeUnit);

    /**
     * Trigger the EPICS apply record. The command return immediately.
     */
    CaCommandMonitor post();

    /**
     * Trigger the EPICS apply record and waits until the command processing
     * completes.
     * 
     * @return an object that implements <code>CaCommandMonitor</code>, which
     *         can be used to monitor the command execution and retrieve its
     *         result.
     */
    CaCommandMonitor postWait() throws InterruptedException;

    /**
     * Trigger the EPICS apply record. The command return immediately.
     * 
     * @param callback
     *            an object that implements <code>CaCommandListener</code>,
     *            which will be notified when the command execution state
     *            changes. The object will be used only for this execution of
     *            the command
     * 
     * @return an object that implements <code>CaCommandMonitor</code>, which
     *         can be used to monitor the command execution and retrieve its
     *         result.
     */
    CaCommandMonitor postCallback(CaCommandListener callback);

    /**
     * Checks if there is a command execution in progress.
     * 
     * @return true if a command is being executed, false otherwise.
     */
    boolean isActive();
    
    /**
     * Clears the MARK flag on all the CADs.
     */
    void clear() throws TimeoutException;

}
