/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.seqexec.server.tcs;

public enum ParkState {

    NOT_PARKED("NOT PARKED"),
    PARKED("PARKED");

    final String name;

    ParkState(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return this.name;
    }

}
