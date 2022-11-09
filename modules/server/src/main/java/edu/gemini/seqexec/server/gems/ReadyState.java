/*
 * Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.seqexec.server.gems;

public enum ReadyState {

    NotReady("Not Ready"), Ready("Ready");

    String name;

    ReadyState(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return this.name;
    }

}
