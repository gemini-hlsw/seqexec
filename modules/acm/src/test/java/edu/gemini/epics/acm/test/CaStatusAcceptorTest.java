/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm.test;

import static org.junit.Assert.*;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import edu.gemini.epics.acm.CaAttribute;
import edu.gemini.epics.acm.CaAttributeListener;
import edu.gemini.epics.acm.CaException;
import edu.gemini.epics.acm.CaService;
import edu.gemini.epics.acm.CaStatusAcceptor;
import gov.aps.jca.CAException;

// TODO: Create a test IOC to run these tests against it.
// For now, just use the TCS simulator.

public final class CaStatusAcceptorTest {

    private static final Logger LOG = LoggerFactory
            .getLogger(CaStatusAcceptorTest.class.getName());

    private static final String CA_ADDR_LIST = "127.0.0.1";
    private static final String TOP = "test";
    private static final String SA_NAME = "sad";
    private static final String ATTR1_NAME = "status1";
    private static final String ATTR1_CHANNEL = TOP + ":"
            + TestSimulator.INTEGER_STATUS;
    private static final String ATTR2_NAME = "status2";
    private static final String ATTR2_CHANNEL = TOP + ":"
            + TestSimulator.STRING_STATUS;
    private static final String ATTR3_NAME = "status3";
    private static final String ATTR3_CHANNEL = TOP + ":"
            + TestSimulator.DOUBLE_STATUS;
    private static final String ATTR4_NAME = "status4";
    private static final String ATTR4_CHANNEL = TOP + ":"
            + TestSimulator.FLOAT_STATUS;

    private static final long SLEEP_TIME = 2000;

    private static TestSimulator simulator;
    private static CaService caService;
    private boolean updated;

    @BeforeClass
    public static void setUp() {
        simulator = new TestSimulator(TOP);
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
    public void testCreateService() {
        assertNotNull("Unable to create CaService.", caService);
    }

    @Test
    public void testCreateStatusAcceptor() {
        CaStatusAcceptor sa = caService.createStatusAcceptor(SA_NAME);

        assertNotNull("Unable to create CaStatusAcceptor.", sa);

        caService.destroyStatusAcceptor(SA_NAME);
    }

    @Test
    public void testGetStatusAcceptor() {
        CaStatusAcceptor sa1 = caService.createStatusAcceptor(SA_NAME);
        CaStatusAcceptor sa2 = caService.getStatusAcceptor(SA_NAME);

        assertEquals("Retrieved the wrong CaStatusAcceptor.", sa1, sa2);

        caService.destroyStatusAcceptor(SA_NAME);
    }

    @Test
    public void testCreateIntegerAttribute() throws CaException, CAException {
        CaStatusAcceptor sa = caService.createStatusAcceptor(SA_NAME);
        CaAttribute<Integer> attr = sa.addInteger(ATTR1_NAME, ATTR1_CHANNEL);

        assertNotNull("Unable to create status acceptor attribute.", attr);

        caService.destroyStatusAcceptor(SA_NAME);
    }

    @Test
    public void testCreateStringAttribute() throws CaException, CAException {
        CaStatusAcceptor sa = caService.createStatusAcceptor(SA_NAME);
        CaAttribute<String> attr = sa.addString(ATTR2_NAME, ATTR2_CHANNEL);

        assertNotNull("Unable to create status acceptor attribute.", attr);

        caService.destroyStatusAcceptor(SA_NAME);
    }

    @Test
    public void testCreateFloatAttribute() throws CaException, CAException {
        CaStatusAcceptor sa = caService.createStatusAcceptor(SA_NAME);
        CaAttribute<Float> attr = sa.addFloat(ATTR4_NAME, ATTR4_CHANNEL);

        assertNotNull("Unable to create status acceptor attribute.", attr);

        caService.destroyStatusAcceptor(SA_NAME);
    }

    @Test
    public void testCreateDoubleAttribute() throws CaException, CAException {
        CaStatusAcceptor sa = caService.createStatusAcceptor(SA_NAME);
        CaAttribute<Double> attr = sa.addDouble(ATTR3_NAME, ATTR3_CHANNEL);

        assertNotNull("Unable to create status acceptor attribute.", attr);

        caService.destroyStatusAcceptor(SA_NAME);
    }

    @Test(expected = CaException.class)
    public void testRejectAttributeCreationWithDifferentType()
            throws CaException, CAException {
        CaStatusAcceptor sa = caService.createStatusAcceptor(SA_NAME);

        sa.addInteger(ATTR1_NAME, ATTR1_CHANNEL);
        try {
            sa.addString(ATTR1_NAME, ATTR1_CHANNEL);
        } finally {
            caService.destroyStatusAcceptor(SA_NAME);
        }
    }

    @Test(expected = CaException.class)
    public void testRejectAttributeCreationWithDifferentChannel()
            throws CaException, CAException {
        CaStatusAcceptor sa = caService.createStatusAcceptor(SA_NAME);
        sa.addInteger(ATTR1_NAME, ATTR1_CHANNEL);
        try {
            sa.addInteger(ATTR1_NAME, ATTR2_CHANNEL);
        } finally {
            caService.destroyStatusAcceptor(SA_NAME);
        }
    }

    @Test
    public void testGetAttribute() throws CaException, CAException {
        CaStatusAcceptor sa = caService.createStatusAcceptor(SA_NAME);
        CaAttribute<Integer> attr1 = sa.addInteger(ATTR1_NAME, ATTR1_CHANNEL);
        CaAttribute<Integer> attr2 = sa.getIntegerAttribute(ATTR1_NAME);

        assertNotNull("Unable to retrieve status acceptor attribute.", attr2);

        assertEquals("Retrieved the wrong status acceptor attribute.", attr1,
                attr2);

        caService.destroyStatusAcceptor(SA_NAME);
    }

    @Test
    public void testGetInfo() throws CaException, CAException {
        CaStatusAcceptor sa = caService.createStatusAcceptor(SA_NAME);
        sa.addInteger(ATTR1_NAME, ATTR1_CHANNEL);
        sa.addString(ATTR2_NAME, ATTR2_CHANNEL);

        Set<String> attrSet = sa.getInfo();

        assertNotNull("Unable to retrieve attribute list.", attrSet);

        Set<String> testSet = new HashSet<>();
        testSet.add(ATTR1_NAME);
        testSet.add(ATTR2_NAME);

        assertEquals("Retrieved bad attribute list.", attrSet, testSet);

        caService.destroyStatusAcceptor(SA_NAME);
    }

    @Test
    public void testAttributeMonitor() throws CaException, CAException {
        CaStatusAcceptor sa = caService.createStatusAcceptor(SA_NAME);
        CaAttribute<Integer> attr = sa.addInteger(ATTR1_NAME, ATTR1_CHANNEL);

        attr.addListener(new CaAttributeListener<Integer>() {

            @Override
            public void onValueChange(List<Integer> newVals) {
                updated = true;
            }

            @Override
            public void onValidityChange(boolean newValidity) {
            }
        });
        try {
            Thread.sleep(SLEEP_TIME);
        } catch (InterruptedException e) {
            LOG.warn(e.getMessage());
        }

        assertTrue("Attribute monitor did not receive updates.", updated);

        caService.destroyStatusAcceptor(SA_NAME);
    }

}
