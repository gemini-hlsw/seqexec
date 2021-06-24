/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
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

public interface CaApplySender extends CaCommandTrigger {
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
     * Retrieves the description for this Apply Sender, or <code>null</code> if none was set.
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
