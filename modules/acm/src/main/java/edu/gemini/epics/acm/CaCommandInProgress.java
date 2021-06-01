/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

/**
 * Exception generated when trying to start a command in an apply record that is
 * already executing another command.
 * 
 * @author jluhrs
 *
 */
public final class CaCommandInProgress extends Exception {

    CaCommandInProgress() {
    }

    CaCommandInProgress(String message) {
        super(message);
    }
}
