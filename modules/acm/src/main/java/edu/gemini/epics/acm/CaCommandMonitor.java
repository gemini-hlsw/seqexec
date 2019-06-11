/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * Define the interface to the objects used to monitor the execution of a
 * command, and to retrieve its results. Command results are a set of named
 * values.
 * 
 * @author jluhrs
 *
 */
public interface CaCommandMonitor {
    enum State {
        BUSY, IDLE, ERROR, PAUSE
    }

    /**
     * Retrieves the completion error, if any.
     * 
     * @return the <code>Exception</code> object that caused the abnormal
     *         completion, or <code>null</code> if the command has not completed
     *         or completed without an error.
     */
    Exception error();

    /**
     * Asks if the command is running.
     * 
     * @return true if the command is running.
     */
    boolean isDone();

    /**
     * Blocks the current thread until the command completes.
     * 
     * @param timeout
     *            time to wait for the command completion, in seconds.
     * @throws TimeoutException
     */
    void waitDone(long timeout, TimeUnit unit) throws TimeoutException,
            InterruptedException;

    void waitDone() throws InterruptedException;

    /**
     * Blocks the current thread while the command is active.
     *
     * @param timeout
     *            time to wait for the command inactivity, in seconds.
     * @throws TimeoutException
     * @return the end state of the command
     */
    State waitInactive(long timeout, TimeUnit unit) throws TimeoutException,
            InterruptedException;

    State waitInactive() throws InterruptedException;

    /**
     * Retrieves the current execution state of the command.
     * 
     * @return the execution state of the command.
     */
    State state();

    /**
     * Sets a listener that will be called when the command completes.
     * 
     * @param cb
     *            the command listener.
     */
    void setCallback(CaCommandListener cb);

}
