/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.seqexec.server.niri;

public enum BeamSplitter {
    F6("f/6"),
    F14("f/14"),
    F32("f/32"),
    SameAsCamera("same-as-camera");

    final String name;

    BeamSplitter(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return this.name;
    }
}
