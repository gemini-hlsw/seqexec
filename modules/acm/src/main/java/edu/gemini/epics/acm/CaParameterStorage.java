/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import edu.gemini.epics.EpicsWriter;
import gov.aps.jca.CAException;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

class CaParameterStorage {
    private final EpicsWriter epicsWriter;
    private final Map<String, CaParameterImpl<String>> stringParameters;
    private final Map<String, CaParameterImpl<Double>> doubleParameters;
    private final Map<String, CaParameterImpl<Float>> floatParameters;
    private final Map<String, CaParameterImpl<Integer>> integerParameters;
    private final Map<String, Object> enumParameters;

    public CaParameterStorage(EpicsWriter epicsWriter) {
        this.epicsWriter = epicsWriter;
        stringParameters = new HashMap<>();
        doubleParameters = new HashMap<>();
        floatParameters = new HashMap<>();
        integerParameters = new HashMap<>();
        enumParameters = new HashMap<>();

    }

    public Set<String> getInfo() {
        Set<String> set = new HashSet<>(doubleParameters.keySet());
        set.addAll(floatParameters.keySet());
        set.addAll(integerParameters.keySet());
        set.addAll(stringParameters.keySet());
        set.addAll(enumParameters.keySet());
        return set;
    }

    public void unbind() {
        assert (epicsWriter != null);

        for (CaParameterImpl<?> param : stringParameters.values()) {
            param.unbind();
        }
        for (CaParameterImpl<?> param : doubleParameters.values()) {
            param.unbind();
        }
        for (CaParameterImpl<?> param : floatParameters.values()) {
            param.unbind();
        }
        for (CaParameterImpl<?> param : integerParameters.values()) {
            param.unbind();
        }
        // FIXME: unbind emun parameters. Use run time casting?

        stringParameters.clear();
        doubleParameters.clear();
        floatParameters.clear();
        integerParameters.clear();
        enumParameters.clear();
    }

    public CaParameter<Integer> addInteger(String name, String channel, String description, boolean isCADParameter)
            throws CaException {
        CaParameterImpl<Integer> param = integerParameters.get(name);
        if (param == null) {
            if (alreadyExist(name)) {
                throw new CaException(
                        "Parameter already exists with a different type.");
            } else {
                param = CaParameterImpl.createIntegerParameter(name, channel,
                        description, epicsWriter, isCADParameter);
                integerParameters.put(name, param);
            }
        } else {
            if (!channel.equals(param.channel())) {
                throw new CaException(
                        "Parameter already exists for a different channel.");
            }
        }
        return param;
    }

    public CaParameter<Double> addDouble(String name, String channel, String description, boolean isCADParameter)
            throws CaException {
        CaParameterImpl<Double> param = doubleParameters.get(name);
        if (param == null) {
            if (alreadyExist(name)) {
                throw new CaException(
                        "Parameter already exists with a different type.");
            } else {
                param = CaParameterImpl.createDoubleParameter(name, channel,
                        description, epicsWriter, isCADParameter);
                doubleParameters.put(name, param);
            }
        } else {
            if (!channel.equals(param.channel())) {
                throw new CaException(
                        "Parameter already exists for a different channel.");
            }
        }
        return param;
    }

    public <T extends Enum<T>, A extends CaParameterImpl<T> > CaParameter<T> addEnum(String name, String channel,
                                                                                     Class<T> enumType,
                                                                                     String description,
                                                                                     boolean isCADParameter)
            throws CaException {
        CaParameterImpl<T> param;
        try {
            param = CaParameterImpl.class.cast(enumParameters.get(name));
        } catch(ClassCastException e) {
            param = null;
        }
        if (param == null) {
            if (alreadyExist(name)) {
                throw new CaException(
                        "Parameter already exists with a different type.");
            } else {
                param = CaParameterImpl.createEnumParameter(name, channel, description, enumType,
                        epicsWriter, isCADParameter);
                enumParameters.put(name, param);
            }
        } else {
            if (!channel.equals(param.channel())) {
                throw new CaException(
                        "Parameter already exists for a different channel.");
            }
        }
        return param;
    }

    public CaParameter<Float> addFloat(String name, String channel, String description, boolean isCADParameter)
            throws CaException {
        CaParameterImpl<Float> param = floatParameters.get(name);
        if (param == null) {
            if (alreadyExist(name)) {
                throw new CaException(
                        "Parameter already exists with a different type.");
            } else {
                param = CaParameterImpl.createFloatParameter(name, channel,
                        description, epicsWriter, isCADParameter);
                floatParameters.put(name, param);
            }
        } else {
            if (!channel.equals(param.channel())) {
                throw new CaException(
                        "Parameter already exists for a different channel.");
            }
        }
        return param;
    }

    public CaParameter<String> addString(String name, String channel, String description, boolean isCADParameter) throws CaException {
         CaParameterImpl<String> param = stringParameters.get(name);
         if (param == null) {
             if (alreadyExist(name)) {
                 throw new CaException(
                         "Parameter already exists with a different type.");
             } else {
                 param = CaParameterImpl.createStringParameter(name, channel,
                         description, epicsWriter);
                 stringParameters.put(name, param);
             }
         } else {
             if (!channel.equals(param.channel())) {
                 throw new CaException(
                         "Parameter already exists for a different channel.");
             }
         }
         return param;
    }

    public CaParameter<Integer> getInteger(String name) {
        return integerParameters.get(name);
    }

    public CaParameter<Double> getDouble(String name) {
        return doubleParameters.get(name);
    }

    public CaParameter<Float> getFloat(String name) {
        return floatParameters.get(name);
    }

    public CaParameter<String> getString(String name) {
        return stringParameters.get(name);
    }

    //FIXME: retrieve Enum parameters.

    public void remove(String name) {
        doubleParameters.remove(name);
        floatParameters.remove(name);
        integerParameters.remove(name);
        stringParameters.remove(name);
        enumParameters.remove(name);
    }

    private boolean alreadyExist(String name) {
        return doubleParameters.containsKey(name)
            || floatParameters.containsKey(name)
            || integerParameters.containsKey(name)
            || stringParameters.containsKey(name)
            || enumParameters.containsKey(name);
    }



}
