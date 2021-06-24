/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

public enum CarState implements CarStateGeneric {
    IDLE, PAUSED, BUSY, ERROR;

    @Override
    public Boolean isIdle() {
        return this == IDLE;
    }

    @Override
    public Boolean isPaused() {
        return this == PAUSED;
    }

    @Override
    public Boolean isBusy() {
        return this == BUSY;
    }

    @Override
    public Boolean isError() {
        return this == ERROR;
    }
}
