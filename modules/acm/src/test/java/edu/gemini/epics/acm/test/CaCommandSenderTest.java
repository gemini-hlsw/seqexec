/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm.test;

import static org.junit.Assert.*;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import edu.gemini.epics.acm.CaApplySender;
import edu.gemini.epics.acm.CaAttribute;
import edu.gemini.epics.acm.CaCommandMonitor;
import edu.gemini.epics.acm.CaCommandSender;
import edu.gemini.epics.acm.CaException;
import edu.gemini.epics.acm.CaParameter;
import edu.gemini.epics.acm.CaService;
import edu.gemini.epics.acm.CaStatusAcceptor;
import gov.aps.jca.CAException;
import gov.aps.jca.TimeoutException;

public final class CaCommandSenderTest {

    private static final Logger LOG = LoggerFactory.getLogger(CaCommandSenderTest.class.getName());

    private static final String CA_ADDR_LIST = "127.0.0.1";
    private static final String TOP = "test";
    private static final String APPLY_NAME = "testApply";
    private static final String CS_NAME = "testCommand";
    private static final String APPLY = TOP + ":apply";
    private static final String CAR = TOP + ":applyC";
    private static final String NORMAL_CAD = TOP + ":test";
    private static final String PARAM1_NAME = "param1";
    private static final String PARAM1_CHANNEL = NORMAL_CAD + ".param1";
    private static final String PARAM2_NAME = "param2";
    private static final String PARAM2_CHANNEL = NORMAL_CAD + ".param2";
    private static final String ERROR_CAD = TOP + ":reboot";
    private static final String TIMEOUT_CAD = TOP + ":init";

    private static final String VALUE = "MagicWord";
    private static final long SLEEP_TIME = 5000;

    private static CaService caService;
    private static TestSimulator simulator;

    @BeforeClass
    public static void setUpClass() {
        simulator = new TestSimulator(TOP);
        simulator.start();
        CaService.setAddressList(CA_ADDR_LIST);
        caService = CaService.getInstance();
    }

    @AfterClass
    public static void tearDownClass() {
        if (caService != null) {
            caService.unbind();
            caService = null;
        }

        if (simulator != null) {
            simulator.stop();
            simulator = null;
        }
    }

    @Before
    public void setUp() throws CAException, TimeoutException {
        //Make sure commands are clear
        CaApplySender apply = caService.createApplySender(APPLY_NAME, APPLY,
                CAR, false);

        apply.clear();

        caService.destroyApplySender(APPLY_NAME);
    }

    @Test
    public void testCreateApplySender() throws CAException {
        CaApplySender apply = caService.createApplySender(APPLY_NAME, APPLY,
                CAR, false);
        assertNotNull("Unable to create CaApplySender", apply);

        caService.destroyApplySender(APPLY_NAME);
    }

    @Test
    public void testGetApplySender() throws CAException {
        CaApplySender apply1 = caService.createApplySender(APPLY_NAME, APPLY,
                CAR, false);
        CaApplySender apply2 = caService.getApplySender(APPLY_NAME);

        assertEquals("Retrieved the wrong CaStatusAcceptor.", apply1, apply2);

        caService.destroyApplySender(APPLY_NAME);
    }

    @Test
    public void testCreateCommandSender() throws CAException {
        CaApplySender apply = caService.createApplySender(APPLY_NAME, APPLY,
                CAR, false);
        CaCommandSender cs = caService.createCommandSender(CS_NAME, apply, null);

        assertNotNull("Unable to create CaCommandSender", cs);

        caService.destroyApplySender(APPLY_NAME);
        caService.destroyCommandSender(CS_NAME);
    }

    @Test
    public void testGetCommandSender() throws CAException {
        CaApplySender apply = caService.createApplySender(APPLY_NAME, APPLY,
                CAR, false);
        CaCommandSender cs1 = caService.createCommandSender(CS_NAME, apply, null);
        CaCommandSender cs2 = caService.getCommandSender(CS_NAME);

        assertEquals("Retrieved the wrong CaStatusAcceptor.", cs1, cs2);

        caService.destroyApplySender(APPLY_NAME);
        caService.destroyCommandSender(CS_NAME);
    }

    @Test
    public void testCreateParameter() throws CaException, CAException {
        CaApplySender apply = caService.createApplySender(APPLY_NAME, APPLY,
                CAR, false);
        CaCommandSender cs = caService.createCommandSender(CS_NAME, apply, null);
        CaParameter<String> param = cs.addString(PARAM1_NAME, PARAM1_CHANNEL);

        assertNotNull("Unable to create CaParameter.", param);

        caService.destroyApplySender(APPLY_NAME);
        caService.destroyCommandSender(CS_NAME);
    }

    @Test(expected = CaException.class)
    public void testRejectParameterCreationWithDifferentType()
            throws CaException, CAException {
        CaApplySender apply = caService.createApplySender(APPLY_NAME, APPLY,
                CAR, false);
        CaCommandSender cs = caService.createCommandSender(CS_NAME, apply, null);

        cs.addString(PARAM1_NAME, PARAM1_CHANNEL);
        cs.addInteger(PARAM1_NAME, PARAM1_CHANNEL);

        caService.destroyApplySender(APPLY_NAME);
        caService.destroyCommandSender(CS_NAME);
    }

    @Test(expected = CaException.class)
    public void testRejectParameterCreationWithDifferentChannel()
            throws CaException, CAException {
        CaApplySender apply = caService.createApplySender(APPLY_NAME, APPLY,
                CAR, false);
        CaCommandSender cs = caService.createCommandSender(CS_NAME, apply, null);

        cs.addString(PARAM1_NAME, PARAM1_CHANNEL);
        cs.addInteger(PARAM1_NAME, PARAM2_CHANNEL);

        caService.destroyApplySender(APPLY_NAME);
        caService.destroyCommandSender(CS_NAME);
    }

    @Test
    public void testGetParameter() throws CaException, CAException {
        CaApplySender apply = caService.createApplySender(APPLY_NAME, APPLY,
                CAR, false);
        CaCommandSender cs = caService.createCommandSender(CS_NAME, apply, null);
        CaParameter<String> param1 = cs.addString(PARAM1_NAME, PARAM1_CHANNEL);
        CaParameter<String> param2 = cs.getString(PARAM1_NAME);

        assertEquals("Retrieved wrong command sender parameter.", param1,
                param2);

        caService.destroyApplySender(APPLY_NAME);
        caService.destroyCommandSender(CS_NAME);
    }

    @Test
    public void testGetInfo() throws CaException, CAException {
        CaApplySender apply = caService.createApplySender(APPLY_NAME, APPLY,
                CAR, false);
        CaCommandSender cs = caService.createCommandSender(CS_NAME, apply, null);
        cs.addString(PARAM1_NAME, PARAM1_CHANNEL);
        cs.addString(PARAM2_NAME, PARAM2_CHANNEL);

        Set<String> paramSet = cs.getInfo();

        assertNotNull("Unable to retrieve attribute list.", paramSet);

        Set<String> testSet = new HashSet<>();
        testSet.add(PARAM1_NAME);
        testSet.add(PARAM2_NAME);

        assertEquals("Retrieved bad attribute list.", paramSet, testSet);

        caService.destroyApplySender(APPLY_NAME);
        caService.destroyCommandSender(CS_NAME);
    }

    @Test
    public void testSetParameter() throws CAException, TimeoutException,
            CaException {
        CaApplySender apply = caService.createApplySender(APPLY_NAME, APPLY,
                CAR, false);
        CaCommandSender cs = caService.createCommandSender(CS_NAME, apply, null);
        CaParameter<String> param1 = cs.addString(PARAM1_NAME, PARAM1_CHANNEL);

        CaStatusAcceptor sa = caService.createStatusAcceptor(CS_NAME);
        CaAttribute<String> attr = sa.addString(PARAM1_NAME, PARAM1_CHANNEL);

        param1.set(VALUE);
        try {
            Thread.sleep(SLEEP_TIME);
        } catch (InterruptedException e) {
            LOG.warn(e.getMessage());
        }
        String val = attr.value();

        assertEquals("Unable to write parameter value.", VALUE, val);

        caService.destroyApplySender(APPLY_NAME);
        caService.destroyCommandSender(CS_NAME);
    }

    @Test
    public void testTriggerUnmarkedCommand() throws CAException,
            TimeoutException, CaException {
        CaApplySender apply = caService.createApplySender(APPLY_NAME, APPLY,
                CAR, false);
        CaCommandSender cs = caService.createCommandSender(CS_NAME, apply, null);

        CaCommandMonitor cm = cs.post();

        try {
            Thread.sleep(SLEEP_TIME);
        } catch (InterruptedException e) {
            LOG.warn(e.getMessage());
        }
        assertTrue("Unmarked command did not completed.", cm.isDone());

        caService.destroyApplySender(APPLY_NAME);
        caService.destroyCommandSender(CS_NAME);
    }

    @Test
    public void testTriggerCommandWithParameter() throws CAException,
            TimeoutException, CaException {
        CaApplySender apply = caService.createApplySender(APPLY_NAME, APPLY,
                CAR, false);
        CaCommandSender cs = caService.createCommandSender(CS_NAME, apply, null);
        CaParameter<String> param1 = cs.addString(PARAM1_NAME, PARAM1_CHANNEL);

        param1.set(VALUE);
        CaCommandMonitor cm = cs.post();

        try {
            Thread.sleep(SLEEP_TIME);
        } catch (InterruptedException e) {
            LOG.warn(e.getMessage());
        }
        assertTrue("Unable to trigger command execution.", cm.isDone());

        caService.destroyApplySender(APPLY_NAME);
        caService.destroyCommandSender(CS_NAME);
    }

    @Test
    public void testTriggerCommandWithMark() throws CAException,
            TimeoutException, CaException {
        CaApplySender apply = caService.createApplySender(APPLY_NAME, APPLY,
                CAR, false);
        CaCommandSender cs = caService.createCommandSender(CS_NAME, apply,
                NORMAL_CAD);

        cs.mark();
        CaCommandMonitor cm = cs.post();

        try {
            Thread.sleep(SLEEP_TIME);
        } catch (InterruptedException e) {
            LOG.warn(e.getMessage());
        }
        assertTrue("Unable to trigger command execution.", cm.isDone());

        caService.destroyApplySender(APPLY_NAME);
        caService.destroyCommandSender(CS_NAME);
    }

    @Test
    public void testCommandError() throws CAException, TimeoutException,
            CaException {
        CaApplySender apply = caService.createApplySender(APPLY_NAME, APPLY,
                CAR, false);
        CaCommandSender cs = caService.createCommandSender(CS_NAME, apply,
                ERROR_CAD);

        cs.mark();
        CaCommandMonitor cm = cs.post();

        try {
            Thread.sleep(SLEEP_TIME);
        } catch (InterruptedException e) {
            LOG.warn(e.getMessage());
        }
        assertTrue("Command did not report execution error.", cm.isDone()
                && cm.state().equals(CaCommandMonitor.State.ERROR));

        caService.destroyApplySender(APPLY_NAME);
        caService.destroyCommandSender(CS_NAME);
    }

    @Test
    public void testRetrieveCommandError() throws CAException,
            TimeoutException, CaException {
        CaApplySender apply = caService.createApplySender(APPLY_NAME, APPLY,
                CAR, false);
        CaCommandSender cs = caService.createCommandSender(CS_NAME, apply,
                ERROR_CAD);

        cs.mark();
        CaCommandMonitor cm = cs.post();

        try {
            Thread.sleep(SLEEP_TIME);
        } catch (InterruptedException e) {
            LOG.warn(e.getMessage());
        }
        //noinspection ThrowableResultOfMethodCallIgnored
        assertTrue("Command did not report execution error.", cm.isDone()
                && cm.state().equals(CaCommandMonitor.State.ERROR)
                && cm.error().getMessage().equals(TestSimulator.ERROR_MSG));

        caService.destroyApplySender(APPLY_NAME);
        caService.destroyCommandSender(CS_NAME);
    }

    @Test
    public void testCommandTimeout() throws CAException, TimeoutException,
            CaException {
        CaApplySender apply = caService.createApplySender(APPLY_NAME, APPLY,
                CAR, false);
        CaCommandSender cs = caService.createCommandSender(CS_NAME, apply,
                TIMEOUT_CAD);

        apply.setTimeout(SLEEP_TIME / 2, TimeUnit.MILLISECONDS);

        cs.mark();
        CaCommandMonitor cm = cs.post();

        try {
            Thread.sleep(SLEEP_TIME);
        } catch (InterruptedException e) {
            LOG.warn(e.getMessage());
        }
        //noinspection ThrowableResultOfMethodCallIgnored
        assertTrue("Command did not report execution timeout (" + cm.isDone() + ", " + cm.state() + ", " + cm.error() + ")",
                cm.isDone() && cm.state().equals(CaCommandMonitor.State.ERROR)
                        && cm.error() instanceof TimeoutException);

        caService.destroyApplySender(APPLY_NAME);
        caService.destroyCommandSender(CS_NAME);
    }

}
