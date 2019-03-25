/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.seqexec.server.niri;

public enum Mask {
    Imaging("imaging"),
    F32_4Pix_Center("0.12'' center"),
    F32_7Pix_Center("0.18'' center"),
    F6_2Pix_Center("0.23'' center"),
    F6_2Pix_Blue("0.23'' blue"),
    F6_4Pix_Center("0.46'' center"),
    F6_4Pix_Blue("0.46'' blue"),
    F6_6Pix_Center("0.70'' center"),
    F6_6Pix_Blue("0.70'' blue"),
    PinHole("pinholemask"),
    NotUsed(""),
    Polarimetry("polarmask");

    String name;

    Mask(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return this.name;
    }

}

