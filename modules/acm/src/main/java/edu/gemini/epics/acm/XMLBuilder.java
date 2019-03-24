/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import edu.gemini.epics.acm.generated.*;
import gov.aps.jca.CAException;

/**
 * Class XMLBuilder builds Command Senders and Status Acceptors, retrieving
 * their parameter definitions from a XML configuration file. It builds the
 * Apply Senders required by the Command Senders automatically.
 *
 * The format of the XML configuration file is defined in file CaSchema.xsd.
 *
 * The prefix of each EPICS channel name defined in the XML configuration file
 * is defined by one of multiple Top element. All the attributes of a Status
 * Acceptor share the same Top. All the parameters of a Command Sender share the
 * same Top, which is the same used by its associated Apply Sender.
 *
 * This class follows the Builder pattern. Configuration methods can be chained.
 *
 * @author jluhrs
 *
 */
public final class XMLBuilder {

    private static final Logger LOG = LoggerFactory.getLogger(XMLBuilder.class.getName());

    private static final String CONFIG_SCHEMA_FILE = "/CaSchema.xsd";
    private static final String XMLSCHEMA_URL = "http://www.w3.org/2001/XMLSchema";

    public XMLBuilder() {
        LOG.info("XMLBuilder created");
        tops = new HashMap<>();
        service = CaService.getInstance();
    }

    /**
     * Loads the Command Senders and Status Acceptors definitions from a file.
     *
     * @param fileName
     *            the file name.
     * @return this XMLBuilder object.
     * @throws FileNotFoundException
     */
    public XMLBuilder fromFile(String fileName) throws FileNotFoundException {
        return fromStream(new FileInputStream(new File(fileName)));
    }

    /**
     * Loads the Command Senders and Status Acceptors from a data stream.
     *
     * @param iStream
     *            the data stream.
     * @return this XMLBuilder object.
     */
    public XMLBuilder fromStream(InputStream iStream) {
        try {
            JAXBContext jc = JAXBContext.newInstance(Records.class);
            Unmarshaller um = jc.createUnmarshaller();
            SchemaFactory factory = SchemaFactory.newInstance(XMLSCHEMA_URL);
            Schema schema = factory.newSchema(this.getClass().getResource(
                    CONFIG_SCHEMA_FILE));
            um.setSchema(schema); // to enable validation
            records = (Records) um.unmarshal(iStream);
        } catch (Exception ex) {
            LOG.warn("XMLBuilder.fromStream: Problem parsing XML");
            throw new IllegalArgumentException("Problem parsing XML", ex);
        }

        for (TopType top : records.getTop()) {
            tops.put(top.getName(), normalizeTop(top.getValue()));
        }

        return this;
    }

    /**
     * Sets the CaService to use for building Command Senders and Status
     * Acceptors
     *
     * @param service
     *            the CaService.
     * @return this XMLBuilder object.
     */
    public XMLBuilder withCaService(CaService service) {
        this.service = service;
        return this;
    }

    /**
     * Sets the value of a named Top, used as a prefix for the EPICS channels
     * names of Command Senders and Status Acceptors created subsequently. If
     * the named Top already exists, its value is overwritten.
     *
     * @param top
     *            the name of the Top.
     * @param value
     *            the value of the Top.
     * @return this XMLBuilder object.
     */
    public XMLBuilder withTop(String top, String value) {
        tops.put(top, normalizeTop(value));
        return this;
    }

    /**
     * Creates a specific Command Sender, using the current configuration.
     *
     * @param commandName
     *            the name of the Command Sender
     * @return the Command Sender, or <code>null</code> if it could not be
     *         created.
     */
    public CaCommandSender buildCommandSender(String commandName) {
        for (ApplyType applyDef : records.getApply()) {
            for (CommandType commandDef : applyDef.getCommand()) {
                if (commandDef.getName().equals(commandName)) {
                    return buildCommand(commandDef, getOrBuildApply(applyDef),
                            tops.get(applyDef.getTop()));
                }
            }
        }
        return null;
    }

    /**
     * Creates a specific Status Acceptor, using the current configuration.
     *
     * @param statusName
     *            the name of the Status Acceptor
     * @return the Status Acceptor, or <code>null</code> if it could not be
     *         created.
     */
    public CaStatusAcceptor buildStatusAcceptor(String statusName) {
        for (StatusType statusDef : records.getStatus()) {
            if (statusDef.getName().equals(statusName)) {
                return buildStatus(statusDef);
            }
        }
        return null;
    }

    /**
     * Creates all the Command Senders and Status Acceptors defined in the
     * current configuration.
     */
    public void buildAll() {
        for (ApplyType applyDef : records.getApply()) {
            CaApplySender apply = getOrBuildApply(applyDef);
            for (CommandType commandDef : applyDef.getCommand()) {
                try {
                    buildCommand(commandDef, apply, tops.get(applyDef.getTop()));
                } catch (Exception e) {
                    LOG.warn(e.getMessage());
                }
            }
        }
        for (StatusType statusDef : records.getStatus()) {
            try {
                buildStatus(statusDef);
            } catch (Exception e) {
                LOG.warn(e.getMessage());
            }
        }
    }

    private Records records;
    private final Map<String, String> tops;
    private CaService service;

    private CaApplySender getOrBuildApply(ApplyType applyDef) {
        CaApplySender apply = service.getApplySender(applyDef.getName());
        if (apply == null) {
            try {
                apply = service.createApplySender(applyDef.getName(),
                        tops.get(applyDef.getTop()) + applyDef.getApply(),
                        tops.get(applyDef.getTop()) + applyDef.getCar(),
                        applyDef.isGem5() != null && applyDef.isGem5(),
                        applyDef.getDescription());
            } catch (CAException e) {
                apply = null;
            }
        }
        return apply;
    }

    private String normalizeTop(String top) {
        if (top.endsWith(":")) {
            return top;
        } else {
            return top + ":";
        }
    }

    private CaCommandSender buildCommand(CommandType commandDef,
            CaApplySender apply, String top) {
        if (apply != null) {
            CaCommandSender cs;
            if (commandDef.getRecord() != null
                    && commandDef.getRecord().length() > 0) {
                cs = service.createCommandSender(commandDef.getName(), apply,
                        top + commandDef.getRecord(),
                        commandDef.getDescription());
            } else {
                cs = service.createCommandSender(commandDef.getName(), apply,
                        null, commandDef.getDescription());
            }
            for (CommandType.Parameter par : commandDef.getParameter()) {
                try {
                    switch (par.getType()) {
                    case DOUBLE:
                        cs.addDouble(par.getName(), top + par.getChannel(),
                                par.getDescription(), par.isIsCAD()==null || par.isIsCAD());
                        break;
                    case FLOAT:
                        cs.addFloat(par.getName(), top + par.getChannel(),
                                par.getDescription(), par.isIsCAD()==null || par.isIsCAD());
                        break;
                    case INTEGER:
                        cs.addInteger(par.getName(), top + par.getChannel(),
                                par.getDescription(), par.isIsCAD()==null || par.isIsCAD());
                        break;
                    case STRING:
                    default:
                        cs.addString(par.getName(), top + par.getChannel(),
                                par.getDescription(), par.isIsCAD()==null || par.isIsCAD());
                        break;

                    }
                } catch (CaException e) {
                    LOG.warn(e.getMessage());
                }
            }
            return cs;
        }
        return null;
    }

    private CaStatusAcceptor buildStatus(StatusType statusDef) {
        CaStatusAcceptor sa = service.createStatusAcceptor(statusDef.getName(),
                statusDef.getDescription());
        String top = tops.get(statusDef.getTop());
        for (StatusType.Attribute attr : statusDef.getAttribute()) {
            String attrTop;
            if (attr.getTop() == null || attr.getTop().length() == 0) {
                attrTop = top;
            } else {
                attrTop = tops.get(attr.getTop());
            }
            try {
                switch (attr.getType()) {
                case DOUBLE:
                    sa.addDouble(attr.getName(), attrTop + attr.getChannel(),
                            attr.getDescription());
                    break;
                case FLOAT:
                    sa.addFloat(attr.getName(), attrTop + attr.getChannel(),
                            attr.getDescription());
                    break;
                case INTEGER:
                    sa.addInteger(attr.getName(), attrTop + attr.getChannel(),
                            attr.getDescription());
                    break;
                case STRING:
                default:
                    sa.addString(attr.getName(), attrTop + attr.getChannel(),
                            attr.getDescription());
                    break;

                }
            } catch (Exception e) {
                LOG.warn(e.getMessage());
            }
        }
        return sa;
    }

}
