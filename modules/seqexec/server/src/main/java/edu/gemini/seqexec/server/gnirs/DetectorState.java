/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.seqexec.server.gnirs;

public enum DetectorState {
    Inactive("Deactivate", false),
    Active("Activate", true); // Why are the values verbs?

    final String name;
    final boolean active;

    DetectorState(String name, boolean active) {
        this.name = name;
        this.active = active;
    }

    @Override
    public String toString() {
        return this.name;
    }

    public boolean getActive() {
        return this.active;
    }
}
