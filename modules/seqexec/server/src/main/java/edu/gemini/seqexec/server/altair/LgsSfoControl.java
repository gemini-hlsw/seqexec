/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.seqexec.server.altair;

public enum LgsSfoControl {

    Disable("disabled"),
    Enable("enabled"),
    Pause("paused");

    final String name;

    LgsSfoControl(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return this.name;
    }
}
