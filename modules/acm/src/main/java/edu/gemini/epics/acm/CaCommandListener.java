/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

/**
 * Defines the interface that must be implemented by clients to monitor the
 * execution of a command.
 * 
 * @author jluhrs
 *
 */
public interface CaCommandListener {
    /**
     * Called when the command completes successfully.
     */
    void onSuccess();

    /**
     * Called when the command completes with an error.
     */
    void onFailure(Exception cause);

    /**
     * Called when the command is paused.
     */
    void onPause();
}
