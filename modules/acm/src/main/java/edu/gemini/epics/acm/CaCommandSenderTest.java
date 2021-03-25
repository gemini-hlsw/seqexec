/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import gov.aps.jca.CAException;
import gov.aps.jca.TimeoutException;

public final class CaCommandSenderTest {
    private static final Logger LOG = LoggerFactory.getLogger(CaCommandSenderTest.class.getName());


    public static void main(String[] args) throws CAException {
        CaService.setAddressList("172.16.2.20");
        CaService service = CaService.getInstance();

        CaApplySender apply;
        apply = service.createApplySender("apply", "tc1:apply", "tc1:applyC", false);

        CaCommandSender cs = service.createCommandSender("sourceA", apply, null);
        try {
            CaParameter<String> parSystem = cs.addString("system", "tc1:sourceA.B");
            CaParameter<String> parCoord1 = cs.addString("theta1", "tc1:sourceA.C");
            CaParameter<String> parCoord2 = cs.addString("theta2", "tc1:sourceA.D");
            parSystem.set("AzEl");
            parCoord1.set(Double.toString(Math.random() * 360.0));
            parCoord2.set(Double.toString(Math.random() * 50.0 + 30.0));
            CaCommandMonitor cm = cs.postCallback(new CaCommandListener() {

                @Override
                public void onSuccess() {
                    System.out.println("Command completed successfully.");
                }

                @Override
                public void onFailure(Exception cause) {
                    System.out.println("Command completed with error " + cause.getMessage());
                }

                @Override
                public void onPause() {
                    System.out.println("Command paused.");
                }
            });
            cm.waitDone();
        } catch (CaException | CAException | TimeoutException | InterruptedException e) {
            // TODO Auto-generated catch block
            LOG.warn(e.getMessage());
        }

        service.unbind();
    }

}
