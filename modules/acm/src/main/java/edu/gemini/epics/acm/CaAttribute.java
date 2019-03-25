/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import java.util.List;

/**
 * Defines the interface to attribute objects. An attributes reads an EPICS
 * channel, and allows to register listener to monitor changes. It monitors the
 * EPICS channel and keeps its most recent value.
 * <p>
 * The value hold by an attribute may not be valid, for example if the
 * connection to the IOC is lost, or if there is a type mismatch between the
 * attribute and the EPICS channel. This is indicated with a flag.
 *
 * @author jluhrs
 *
 * @param <T>
 *            type of the attribute. Allowed types are Double, Float, Integer
 *            and String.
 */
public interface CaAttribute<T> {
    /**
     * Retrieves the name of this attribute.
     *
     * @return the name of this attribute.
     */
    String name();

    /**
     * Retrieves the name of the channel to which this attribute is connected.
     *
     * @return the name of the EPICS channel.
     */
    String channel();

    /**
     * Retrieves the description of this attribute, or <code>null</code> if none was set.
     *
     * @return the description of this attribute
     */
    String description();

    /**
     * Retrieves the most recent value read from the EPICS channel, or
     * <code>null</code> if no value has been read yet or the attribute is
     * invalid. If the EPICS channel contains an array, the fist element is
     * returned.
     *
     * @return the EPICS channel value.
     */
    T value();

    /**
     * Retrieves the most recent value read from the EPICS channel, as a list,
     * or <code>null</code> if no value has been read yet or the attribute is
     * invalid. Useful if the EPICS channel contains an array.
     *
     * @return the EPICS channel value.
     */
    List<T> values();

    /**
     * Shows if this attribute's value is valid.
     *
     * @return the validity flag.
     */
    boolean valid();

    /**
     * Register a listener to monitor this attribute.
     *
     * @param listener
     *            the listener that will receive the update notifications.
     */
    void addListener(CaAttributeListener<T> listener);

    /**
     * Unregister a listener (optional operation)
     *
     * @param listener
     *            the listener that will be unregistered.
     */
    void removeListener(CaAttributeListener<T> listener);
}
