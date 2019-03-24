/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import java.util.List;

/**
 * Defines the interface that must be implemented by clients to monitor an
 * attribute.
 * 
 * @author jluhrs
 *
 * @param <T>
 *            The type of the attribute that will be monitored.
 */
public interface CaAttributeListener<T> {
    /**
     * Called when the monitored attribute refreshes its value.
     * 
     * @param newVals
     *            the attributes's new value. It can be <code>null</code>.
     */
    void onValueChange(List<T> newVals);

    /**
     * Called when the validity state of the attribute's value changes
     * 
     * @param newValidity
     *            the new validity state.
     */
    void onValidityChange(boolean newValidity);
}
