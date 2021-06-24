/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

/**
 * Created by jluhrs on 10/18/17.
 */
public class CaObserveAborted extends Exception {
    CaObserveAborted() {
    }

    CaObserveAborted(String message) {
        super(message);
    }
}
