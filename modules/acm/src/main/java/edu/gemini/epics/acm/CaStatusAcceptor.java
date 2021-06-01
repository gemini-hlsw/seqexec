/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import gov.aps.jca.CAException;

import java.util.Set;

/**
 * Defines the interface to status acceptor objects. An status acceptor is a
 * named collection of attributes that can be read from a IOC.
 * 
 * @author jluhrs
 *
 */
public interface CaStatusAcceptor {
    /**
     * Retrieves the name of this status acceptor.
     * 
     * @return name of this status acceptor.
     */
    String getName();

    /**
     * Retrieves the description for this Status Acceptor
     * 
     * @return the description of this Status Acceptor
     */
    String getDescription();


    /**
     * Retrieve the names of the attributes of this status acceptor.
     * 
     * @return set of attribute names.
     */
    Set<String> getInfo();

    /**
     * Adds an attribute of type <code>Double</code> to this status acceptor. If
     * the attribute already exists, the existing object is used. CaException is
     * thrown if the existing attribute is of a different type or uses a
     * different EPICS channel.
     * 
     * @param name
     *            the name of the attribute.
     * @param channel
     *            the full EPICS channel name for the attribute.
     * @param description
     *            optional description of the attribute.
     * @return the attribute.
     * @throws CaException
     * @throws CAException
     */
    CaAttribute<Double> addDouble(String name, String channel, String description)
            throws CaException, CAException;
    CaAttribute<Double> addDouble(String name, String channel)
            throws CaException, CAException;

    /**
     * Adds an attribute of type <code>Float</code> to this status acceptor. If
     * the attribute already exists, the existing object is used. CaException is
     * thrown if the existing attribute is of a different type or uses a
     * different EPICS channel.
     * 
     * @param name
     *            the name of the attribute.
     * @param channel
     *            the full EPICS channel name for the attribute.
     * @param description
     *            optional description of the attribute.
     * @return the attribute.
     * @throws CaException
     * @throws CAException
     */
    CaAttribute<Float> addFloat(String name, String channel, String description)
            throws CaException, CAException;
    CaAttribute<Float> addFloat(String name, String channel)
            throws CaException, CAException;

    /**
     * Adds an attribute of type <code>Integer</code> to this status acceptor.
     * If the attribute already exists, the existing object is used. CaException
     * is thrown if the existing attribute is of a different type or uses a
     * different EPICS channel.
     * 
     * @param name
     *            the name of the attribute.
     * @param channel
     *            the full EPICS channel name for the attribute.
     * @param description
     *            optional description of the attribute.
     * @return the attribute.
     * @throws CaException
     * @throws CAException
     */
    CaAttribute<Integer> addInteger(String name, String channel, String description)
            throws CaException, CAException;
    CaAttribute<Integer> addInteger(String name, String channel)
            throws CaException, CAException;

    /**
     * Adds an attribute of type <code>Short</code> to this status acceptor.
     * If the attribute already exists, the existing object is used. CaException
     * is thrown if the existing attribute is of a different type or uses a
     * different EPICS channel.
     *
     * @param name
     *            the name of the attribute.
     * @param channel
     *            the full EPICS channel name for the attribute.
     * @param description
     *            optional description of the attribute.
     * @return the attribute.
     * @throws CaException
     * @throws CAException
     */
    CaAttribute<Short> addShort(String name, String channel, String description)
            throws CaException, CAException;
    CaAttribute<Short> addShort(String name, String channel)
            throws CaException, CAException;

    /**
     * Adds an attribute of type <code>String</code> to this status acceptor. If
     * the attribute already exists, the existing object is used. CaException is
     * thrown if the existing attribute is of a different type or uses a
     * different EPICS channel.
     * 
     * @param name
     *            the name of the attribute.
     * @param channel
     *            the full EPICS channel name for the attribute.
     * @param description
     *            optional description of the attribute.
     * @return the attribute.
     * @throws CaException
     * @throws CAException
     */
    CaAttribute<String> addString(String name, String channel, String description)
            throws CaException, CAException;
    CaAttribute<String> addString(String name, String channel)
            throws CaException, CAException;

    /**
     * Adds an attribute of type <code>Enum</code> to this status acceptor. If
     * the attribute already exists, the existing object is used. CaException is
     * thrown if the existing attribute is of a different type or uses a
     * different EPICS channel.
     *
     * @param name
     *            the name of the attribute.
     * @param channel
     *            the full EPICS channel name for the attribute.
     * @param description
     *            optional description of the attribute.
     * @return the attribute.
     * @throws CaException
     * @throws CAException
     */
    <T extends Enum<T>> CaAttribute<T> addEnum(String name, String channel, Class<T> enumType, String description)
            throws CaException, CAException;
    <T extends Enum<T>> CaAttribute<T> addEnum(String name, String channel, Class<T> enumType)
            throws CaException, CAException;

    /**
     * Removes and existing attribute (optional operation).
     * 
     * @param name
     *            the name of the attribute to be removed.
     */
    void remove(String name);

    /**
     * Retrieves an existing attribute of type <code>Double</code>.
     * 
     * @param name
     *            the name of the attribute.
     * @return the attribute, or <code>null</code> if it does not exist or is of
     *         a different type.
     */
    CaAttribute<Double> getDoubleAttribute(String name);

    /**
     * Retrieves an existing attribute of type <code>Float</code>.
     * 
     * @param name
     *            the name of the attribute.
     * @return the attribute, or <code>null</code> if it does not exist or is of
     *         a different type.
     */
    CaAttribute<Float> getFloatAttribute(String name);

    /**
     * Retrieves an existing attribute of type <code>Integer</code>.
     * 
     * @param name
     *            the name of the attribute.
     * @return the attribute, or <code>null</code> if it does not exist or is of
     *         a different type.
     */
    CaAttribute<Integer> getIntegerAttribute(String name);

    /**
     * Retrieves an existing attribute of type <code>Short</code>.
     *
     * @param name
     *            the name of the attribute.
     * @return the attribute, or <code>null</code> if it does not exist or is of
     *         a different type.
     */
    CaAttribute<Short> getShortAttribute(String name);

    /**
     * Retrieves an existing attribute of type <code>String</code>.
     * 
     * @param name
     *            the name of the attribute.
     * @return the attribute, or <code>null</code> if it does not exist or is of
     *         a different type.
     */
    CaAttribute<String> getStringAttribute(String name);
}
