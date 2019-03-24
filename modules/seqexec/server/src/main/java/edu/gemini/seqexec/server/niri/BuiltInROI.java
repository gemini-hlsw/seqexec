/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.seqexec.server.niri;

public enum BuiltInROI {
    FullFrame("1024"),
    Central768("768"),
    Central512("512"),
    Central256("256"),
    Central128("128"),
    Spec1024x512("1024x512");

    final String name;

    BuiltInROI(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return this.name;
    }
}
