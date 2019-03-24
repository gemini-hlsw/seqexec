/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

public interface CaCommandTrigger {
    /**
     * Trigger the EPICS apply record. The command return immediately.
     *
     * @return an object that implements <code>CaCommandMonitor</code>, which
     *         can be used to monitor the command execution and retrieve its
     *         result.
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

}
