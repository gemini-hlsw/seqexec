/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.seqexec.server.niri;

public enum ReadMode {
    LowRN("low RN (12e)"),
    MedRN("med RN (50e)"),
    HighRN("high RN (200e)"),
    ThermalIR("thermal IR"),
    MedRNDeep("med RN (deep)");

    String name;

    ReadMode(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return this.name;
    }

}

