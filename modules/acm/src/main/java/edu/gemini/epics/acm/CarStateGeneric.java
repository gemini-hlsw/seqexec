/*
 * Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

public interface CarStateGeneric {
    Boolean isIdle();
    Boolean isPaused();
    Boolean isBusy();
    Boolean isError();
}
