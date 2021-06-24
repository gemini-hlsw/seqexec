/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import gov.aps.jca.TimeoutException;

import java.util.Set;

/**
 * @author jluhrs
 *
 */
public interface CaCommandSender extends CaCommandTrigger {
    /**
     * Retrieves the name of this command sender.
     *
     * @return name of this command sender.
     */
    String getName();

    /**
     * Retrieves the description for this Command Sender, or <code>null</code> if none was set.
     *
     * @return the description of this Command Sender
     */
    String getDescription();

    /**
     * Retrieves the names of the parameters of this command sender.
     *
     * @return set of parameter names.
     */
    Set<String> getInfo();

    /**
     * Access the associated CaApplySender object.
     *
     * @return set of parameter names.
     */
    CaApplySender getApplySender();


    /**
     * Marks this command for execution. Useful for commands without parameters.
     */
    void mark() throws TimeoutException;

    /**
     * Clears the MARK flag for this command for execution.
     */
    void clear() throws TimeoutException;

    /**
     * Adds a parameter of type <code>Integer</code> to this command sender. If
     * the parameter already exist, the existing object is used. CaException is
     * thrown if the existing parameter is of a different type or uses a
     * different EPICS channel.
     *
     * @param name
     *            the name of the parameter.
     * @param channel
     *            the full EPICS channel name for the parameter
     * @param description
     *            optional description for the parameter
     * @return the parameter
     * @throws CaException
     */
    CaParameter<Integer> addInteger(String name, String channel,
                                    String description, boolean isCADParameter) throws CaException;

    default CaParameter<Integer> addInteger(String name, String channel)
            throws CaException {
        return addInteger(name, channel, null, true);
    }

    default CaParameter<Integer> addInteger(String name, String channel, boolean isCADParameter) throws CaException {
        return addInteger(name, channel, null, isCADParameter);
    }

    default CaParameter<Integer> addInteger(String name, String channel,
            String description) throws CaException {
        return addInteger(name, channel, description, true);
    }

    /**
     * Adds a parameter of type <code>Double</code> to this command sender. If
     * the parameter already exist, the existing object is used. CaException is
     * thrown if the existing parameter is of a different type or uses a
     * different EPICS channel.
     *
     * @param name
     *            the name of the parameter.
     * @param channel
     *            the full EPICS channel name for the parameter
     * @param description
     *            optional description for the parameter
     * @return the parameter
     * @throws CaException
     */
    CaParameter<Double> addDouble(String name, String channel,
                                  String description, boolean isCADParameter) throws CaException;

    default CaParameter<Double> addDouble(String name, String channel)
            throws CaException {
        return addDouble(name, channel, null, true);
    }

    default CaParameter<Double> addDouble(String name, String channel, boolean isCADParameter) throws CaException {
        return addDouble(name, channel, null, isCADParameter);
    }

    default CaParameter<Double> addDouble(String name, String channel,
            String description) throws CaException {
        return addDouble(name, channel, description, true);
    }


    /**
     * Adds a parameter of type <code>Float</code> to this command sender. If
     * the parameter already exist, the existing object is used. CaException is
     * thrown if the existing parameter is of a different type or uses a
     * different EPICS channel.
     *
     * @param name
     *            the name of the parameter.
     * @param channel
     *            the full EPICS channel name for the parameter
     * @param description
     *            optional description for the parameter
     * @return the parameter
     * @throws CaException
     */
    CaParameter<Float> addFloat(String name, String channel,
                                String description, boolean isCADParameter) throws CaException;

    default CaParameter<Float> addFloat(String name, String channel)
            throws CaException {
        return addFloat(name, channel, null, true);
    }

    default CaParameter<Float> addFloat(String name, String channel, boolean isCADParameter) throws CaException {
        return addFloat(name, channel, null, isCADParameter);
    }

    default CaParameter<Float> addFloat(String name, String channel,
            String description) throws CaException {
        return addFloat(name, channel, description, true);
    }

    /**
     * Adds a parameter of type <code>Enum</code> to this command sender. If
     * the parameter already exist, the existing object is used. CaException is
     * thrown if the existing parameter is of a different type or uses a
     * different EPICS channel.
     *
     * @param name
     *            the name of the parameter.
     * @param channel
     *            the full EPICS channel name for the parameter
     * @param description
     *            optional description for the parameter
     * @return the parameter
     * @throws CaException
     */
    <T extends Enum<T>> CaParameter<T> addEnum(String name, String channel, Class<T> enumType,
                                String description, boolean isCADParameter) throws CaException;

    default <T extends Enum<T>> CaParameter<T> addEnum(String name, String channel, Class<T> enumType,
                                                       String description) throws CaException {
        return addEnum(name, channel, enumType, description, true);
    }

    default <T extends Enum<T>> CaParameter<T> addEnum(String name, String channel, Class<T> enumType)
            throws CaException {
        return addEnum(name, channel, enumType, null, true);
    }

    default <T extends Enum<T>> CaParameter<T> addEnum(String name, String channel, Class<T> enumType,
                                                       boolean isCADParameter) throws CaException {
        return addEnum(name, channel, enumType, null, isCADParameter);
    }

    /**
     * Adds a parameter of type <code>String</code> to this command sender. If
     * the parameter already exist, the existing object is used. CaException is
     * thrown if the existing parameter is of a different type or uses a
     * different EPICS channel.
     *
     * @param name
     *            the name of the parameter.
     * @param channel
     *            the full EPICS channel name for the parameter
     * @param description
     *            optional description for the parameter
     * @return the parameter
     * @throws CaException
     */
    CaParameter<String> addString(String name, String channel,
                                  String description, boolean isCADParameter) throws CaException;

    default CaParameter<String> addString(String name, String channel)
            throws CaException {
        return addString(name, channel, null, true);
    }

    default CaParameter<String> addString(String name, String channel, boolean isCADParameter) throws CaException {
        return addString(name, channel, null, isCADParameter);
    }

    default CaParameter<String> addString(String name, String channel,
            String description) throws CaException {
        return addString(name, channel, description, true);
    }

    /**
     * Removes a parameter from this command sender (optional operation).
     *
     * @param name
     *            the name of the parameter to remove.
     */
    void remove(String name);

    /**
     * Retrieves an existing parameter of type <code>Integer</code>.
     *
     * @param name
     *            the name of the parameter.
     * @return the parameter, or <code>null</code> if it does not exist or is of
     *         a different type.
     */
    CaParameter<Integer> getInteger(String name);

    /**
     * Retrieves an existing parameter of type <code>Double</code>.
     *
     * @param name
     *            the name of the parameter.
     * @return the parameter, or <code>null</code> if it does not exist or is of
     *         a different type.
     */
    CaParameter<Double> getDouble(String name);

    /**
     * Retrieves an existing parameter of type <code>Float</code>.
     *
     * @param name
     *            the name of the parameter.
     * @return the parameter, or <code>null</code> if it does not exist or is of
     *         a different type.
     */
    CaParameter<Float> getFloat(String name);

    /**
     * Retrieves an existing parameter of type <code>String</code>.
     *
     * @param name
     *            the name of the parameter.
     * @return the parameter, or <code>null</code> if it does not exist or is of
     *         a different type.
     */
    CaParameter<String> getString(String name);

}
