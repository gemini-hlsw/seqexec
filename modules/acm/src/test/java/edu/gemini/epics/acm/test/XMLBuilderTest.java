/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm.test;

import static org.junit.Assert.*;

import java.io.FileNotFoundException;
import java.util.Set;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import edu.gemini.epics.acm.CaCommandSender;
import edu.gemini.epics.acm.CaService;
import edu.gemini.epics.acm.CaStatusAcceptor;
import edu.gemini.epics.acm.XMLBuilder;

public final class XMLBuilderTest {

    private static final String CA_ADDR_LIST = "127.0.0.1";
    private static final String TOP1 = "test";
    private static final String CA_CONFIG_FILE = "/TestConfig.xml";
    private static final String STATUS1 = "testStatus";
    private static final String ATTRIBUTE[] = { "dblStatus", "intStatus",
            "fltStatus", "strStatus" };
    private static final String COMMAND1 = "test";
    private static final String PARAM[] = { "param1", "param2" };

    private static TestSimulator simulator;
    private static CaService caService;

    @BeforeClass
    public static void setUp() {
        simulator = new TestSimulator(TOP1);
        simulator.start();
        CaService.setAddressList(CA_ADDR_LIST);
        caService = CaService.getInstance();
    }

    @AfterClass
    public static void tearDown() {
        if (caService != null) {
            caService.unbind();
            caService = null;
        }
        if (simulator != null) {
            simulator.stop();
            simulator = null;
        }
    }

    @Test
    public void testCreateStatus() throws FileNotFoundException {
        XMLBuilder builder = new XMLBuilder();

        CaStatusAcceptor sa = builder
                .withCaService(caService)
                .fromFile(XMLBuilderTest.class.getResource(CA_CONFIG_FILE).getPath())
                .buildStatusAcceptor(STATUS1);

        assertNotNull("Unable to create status acceptor.", sa);

        Set<String> saAttribs = sa.getInfo();

        assertTrue("Status acceptor created with wrong attributes.",
                saAttribs.size() == ATTRIBUTE.length);
        for (String attrName : ATTRIBUTE) {
            assertTrue("Status acceptor created with wrong attributes.",
                    saAttribs.contains(attrName));
        }

        caService.destroyStatusAcceptor(sa.getName());
    }
    
    @Test
    public void testCreateCommand() throws FileNotFoundException {
        XMLBuilder builder = new XMLBuilder();

        CaCommandSender cs = builder
                .withCaService(caService)
                .fromFile(XMLBuilderTest.class.getResource(CA_CONFIG_FILE).getPath())
                .buildCommandSender(COMMAND1);

        assertNotNull("Unable to create status acceptor.", cs);

        Set<String> csParams = cs.getInfo();

        assertTrue("Status acceptor created with wrong attributes.",
                csParams.size() == PARAM.length);
        for (String paramName : PARAM) {
            assertTrue("Status acceptor created with wrong attributes.",
                    csParams.contains(paramName));
        }

        caService.destroyCommandSender(cs.getName());
    }

}
