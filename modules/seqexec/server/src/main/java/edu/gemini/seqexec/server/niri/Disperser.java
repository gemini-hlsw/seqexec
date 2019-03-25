/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.seqexec.server.niri;

public enum Disperser {
    None("none"),
    J("J-grism"),
    H("H-grism"),
    K("K-grism"),
    L("L-grism"),
    M("M-grism"),
    F32_J("f32 J-grism"),
    F32_H("f32 H-grism"),
    F32_K("f32 K-grism"),
    NotUsed(""),
    Wollaston("Wollaston");

    final String name;

    Disperser(String name) {
       this.name = name;
   }

    @Override
    public String toString() {
       return this.name;
   }
}
