/*
 * Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.seqexec.server.niri;

public enum Disperser {
    J("J-grism"),
    H("H-grism"),
    K("K-grism"),
    L("L-grism"),
    M("M-grism"),
    none("none");

    final String name;

    Disperser(String name) {
       this.name = name;
   }

    @Override
    public String toString() {
       return this.name;
   }
}
