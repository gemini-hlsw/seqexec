/*
 * Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import edu.gemini.epics.EpicsService;
import edu.gemini.epics.EpicsReader;
import edu.gemini.epics.impl.EpicsReaderImpl;
import gov.aps.jca.CAException;

final class CaStatusAcceptorImpl implements CaStatusAcceptor {

    private static final Logger LOG = LoggerFactory.getLogger(CaStatusAcceptorImpl.class.getName());

    private final String name;
    private final String description;
    private final Map<String, CaAttributeImpl<String>> stringAttributes;
    private final Map<String, CaAttributeImpl<Double>> doubleAttributes;
    private final Map<String, CaAttributeImpl<Float>> floatAttributes;
    private final Map<String, CaAttributeImpl<Integer>> integerAttributes;
    private final Map<String, CaAttributeImpl<Short>> shortAttributes;
    private final Map<String, Object> enumAttributes;
    private EpicsReader epicsReader;

    public CaStatusAcceptorImpl(String name, String description,
            EpicsService epicsService) {
        super();
        this.name = name;
        this.description = description;
        stringAttributes = new HashMap<>();
        doubleAttributes = new HashMap<>();
        floatAttributes = new HashMap<>();
        integerAttributes = new HashMap<>();
        shortAttributes = new HashMap<>();
        enumAttributes = new HashMap<>();
        epicsReader = new EpicsReaderImpl(epicsService);
    }

    @Override
    public CaAttribute<Double> addDouble(String name, String channel)
            throws CaException, CAException {
        return addDouble(name, channel, null);
    }

    @Override
    public CaAttribute<Double> addDouble(String name, String channel, String description)
            throws CaException, CAException {
        CaAttributeImpl<Double> attr = doubleAttributes.get(name);
        if (attr == null) {
            if (alreadyExist(name)) {
                throw new CaException(
                        "Attribute already exists with a different type.");
            } else {
                attr = CaAttributeImpl.createDoubleAttribute(name, channel, description,
                        epicsReader);
                doubleAttributes.put(name, attr);
            }
        } else {
            if (!channel.equals(attr.channel())) {
                throw new CaException(
                        "Attribute already exists for a different channel.");
            }
        }
        return attr;
    }

    @Override
    public CaAttribute<Float> addFloat(String name, String channel)
            throws CaException, CAException {
        return addFloat(name, channel, null);
    }

    @Override
    public CaAttribute<Float> addFloat(String name, String channel, String description)
            throws CaException, CAException {
        CaAttributeImpl<Float> attr = floatAttributes.get(name);
        if (attr == null) {
            if (alreadyExist(name)) {
                throw new CaException(
                        "Attribute already exists with a different type.");
            } else {
                attr = CaAttributeImpl.createFloatAttribute(name, channel, description,
                        epicsReader);
                floatAttributes.put(name, attr);
            }
        } else {
            if (!channel.equals(attr.channel())) {
                throw new CaException(
                        "Attribute already exists for a different channel.");
            }
        }
        return attr;
    }

    @Override
    public CaAttribute<Integer> addInteger(String name, String channel)
            throws CaException, CAException {
        return addInteger(name, channel, null);
    }

    @Override
    public CaAttribute<Integer> addInteger(String name, String channel, String description)
            throws CaException, CAException {
        CaAttributeImpl<Integer> attr = integerAttributes.get(name);
        if (attr == null) {
            if (alreadyExist(name)) {
                throw new CaException(
                        "Attribute already exists with a different type.");
            } else {
                attr = CaAttributeImpl.createIntegerAttribute(name, channel, description,
                        epicsReader);
                integerAttributes.put(name, attr);
            }
        } else {
            if (!channel.equals(attr.channel())) {
                throw new CaException(
                        "Attribute already exists for a different channel.");
            }
        }
        return attr;
    }

    @Override
    public CaAttribute<Short> addShort(String name, String channel)
            throws CaException, CAException {
        return addShort(name, channel, null);
    }

    @Override
    public CaAttribute<Short> addShort(String name, String channel, String description)
            throws CaException, CAException {
        CaAttributeImpl<Short> attr = shortAttributes.get(name);
        if (attr == null) {
            if (alreadyExist(name)) {
                throw new CaException(
                        "Attribute already exists with a different type.");
            } else {
                attr = CaAttributeImpl.createShortAttribute(name, channel, description,
                        epicsReader);
                shortAttributes.put(name, attr);
            }
        } else {
            if (!channel.equals(attr.channel())) {
                throw new CaException(
                        "Attribute already exists for a different channel.");
            }
        }
        return attr;
    }

    @Override
    public CaAttribute<String> addString(String name, String channel)
            throws CaException, CAException {
        return addString(name, channel, null);
    }

    @Override
    public <T extends Enum<T> > CaAttribute<T> addEnum(String name, String channel, Class<T> enumType,
                                                      String description) throws CaException, CAException {
        return addEnum(name, channel, enumType, CaAttributeImpl.class, description);
    }

    private <T extends Enum<T>, A extends CaAttributeImpl<T> > CaAttribute<T> addEnum(String name, String channel,
                                                                                      Class<T> enumType,
                                                                                      Class<A> attrType,
                                                                                      String description)
            throws CaException, CAException {
        CaAttributeImpl<T> attr;
        try {
            attr = attrType.cast(enumAttributes.get(name));
        } catch(ClassCastException e) {
            attr = null;
        }
        if (attr == null) {
            if (alreadyExist(name)) {
                throw new CaException(
                        "Attribute already exists with a different type.");
            } else {
                attr = CaAttributeImpl.createEnumAttribute(name, channel, description, enumType,
                        epicsReader);
                enumAttributes.put(name, attr);
            }
        } else {
            if (!channel.equals(attr.channel())) {
                throw new CaException(
                        "Attribute already exists for a different channel.");
            }
        }
        return attr;
    }

    @Override
    public <T extends Enum<T>> CaAttribute<T> addEnum(String name, String channel, Class<T> enumType) throws CaException, CAException {
        return addEnum(name, channel, enumType, null);
    }

    @Override
    public CaAttribute<String> addString(String name, String channel, String description)
            throws CaException, CAException {
        CaAttributeImpl<String> attr = stringAttributes.get(name);
        if (attr == null) {
            if (alreadyExist(name)) {
                throw new CaException(
                        "Attribute already exists with a different type.");
            } else {
                attr = CaAttributeImpl.createStringAttribute(name, channel, description,
                        epicsReader);
                stringAttributes.put(name, attr);
            }
        } else {
            if (!channel.equals(attr.channel())) {
                throw new CaException(
                        "Attribute already exists for a different channel.");
            }
        }
        return attr;
    }

    private boolean alreadyExist(String name) {
         return doubleAttributes.containsKey(name)
                || floatAttributes.containsKey(name)
                || integerAttributes.containsKey(name)
                || shortAttributes.containsKey(name)
                || stringAttributes.containsKey(name)
                || enumAttributes.containsKey(name);
    }

    @Override
    public void remove(String name) {
        doubleAttributes.remove(name);
        floatAttributes.remove(name);
        integerAttributes.remove(name);
        shortAttributes.remove(name);
        stringAttributes.remove(name);
        enumAttributes.remove(name);
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Set<String> getInfo() {
        Set<String> set = new HashSet<>(doubleAttributes.keySet());
        set.addAll(floatAttributes.keySet());
        set.addAll(integerAttributes.keySet());
        set.addAll(shortAttributes.keySet());
        set.addAll(stringAttributes.keySet());
        set.addAll(enumAttributes.keySet());
        return set;
    }

    @Override
    public CaAttribute<String> getStringAttribute(String name) {
        return stringAttributes.get(name);
    }

    @Override
    public CaAttribute<Double> getDoubleAttribute(String name) {
        return doubleAttributes.get(name);
    }

    @Override
    public CaAttribute<Float> getFloatAttribute(String name) {
        return floatAttributes.get(name);
    }

    @Override
    public CaAttribute<Integer> getIntegerAttribute(String name) {
        return integerAttributes.get(name);
    }

    @Override
    public CaAttribute<Short> getShortAttribute(String name) {
        return shortAttributes.get(name);
    }

    public void unbind() {
        assert (epicsReader != null);

        for (CaAttributeImpl<?> attr : stringAttributes.values()) {
            try {
                attr.unbind();
            } catch (CAException e) {
                LOG.warn(e.getMessage());
            }
        }
        for (CaAttributeImpl<?> attr : doubleAttributes.values()) {
            try {
                attr.unbind();
            } catch (CAException e) {
                LOG.warn(e.getMessage());
            }
        }
        for (CaAttributeImpl<?> attr : floatAttributes.values()) {
            try {
                attr.unbind();
            } catch (CAException e) {
                LOG.warn(e.getMessage());
            }
        }
        for (CaAttributeImpl<?> attr : integerAttributes.values()) {
            try {
                attr.unbind();
            } catch (CAException e) {
                LOG.warn(e.getMessage());
            }
        }
        for (CaAttributeImpl<?> attr : shortAttributes.values()) {
            try {
                attr.unbind();
            } catch (CAException e) {
                LOG.warn(e.getMessage());
            }
        }

        stringAttributes.clear();
        doubleAttributes.clear();
        floatAttributes.clear();
        integerAttributes.clear();
        shortAttributes.clear();
        enumAttributes.clear();

        epicsReader = null;
    }

    @Override
    public String getDescription() {
        return description;
    }

}
