/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import gov.aps.jca.CAException;
import gov.aps.jca.TimeoutException;

/**
 * Defines the interface to parameter objects. A parameter is used to write a
 * value to an EPICS channel, specifically to a CAD input.
 *
 * @author jluhrs
 *
 * @param <T>
 *            type of the parameter. Allowed types are Double, Float, Integer
 *            and String.
 */
public interface CaParameter<T> {
    /**
     * Retrieves the name of this parameter.
     *
     * @return the name of this parameter.
     */
    String name();

    /**
     * Retreives the name of the EPICS channel associated to this parameter.
     *
     * @return the name of the EPICS channel.
     */
    String channel();

    /**
     * Retrieves the description for this parameter, or <code>null</code> if none was set.
     *
     * @return the description of this parameter
     */
    String description();

    /**
     * Retrieves the value of the parameter. This is the last value assigned to
     * the parameter, not the value currently held by the EPICS channel.
     *
     * @return the value of the parameter, <code>null</code> if no value has
     *         been assigned.
     */
    T value();

    void set(T value) throws CAException, TimeoutException;
}
