/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

/**
 * This Exception class is used to indicate that a remote EPICS systems has
 * completed a command with an error. It allows to retrieve the error message
 * provided by remote EPICS system.
 *
 * @author jluhrs
 *
 */
public final class CaCommandError extends Exception {

    CaCommandError() {
    }

    public CaCommandError(String message) {
        super(message);
    }
}
