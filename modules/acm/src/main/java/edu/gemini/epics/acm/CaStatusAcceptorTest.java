/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import gov.aps.jca.CAException;

import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

final class CaStatusAcceptorTest {
    
    private static final Logger LOG = LoggerFactory.getLogger(CaStatusAcceptorTest.class.getName());

    public static void main(String[] args) {
        CaService service = CaService.getInstance();

        CaStatusAcceptor sa = service.createStatusAcceptor("tcs::sad");
        try {
            sa.addString("LST", "tcs:LST.VAL");
        } catch (CaException | CAException e) {
            LOG.warn(e.getMessage());
        }
        CaAttribute<String> lst = sa.getStringAttribute("LST");

        for (int i = 0; i < 5; i++) {
            try {
                Thread.sleep(750);
            } catch (InterruptedException e) {
                LOG.warn(e.getMessage());
            }
            System.out.println(i + ": " + lst.value());
        }

        lst.addListener(new CaAttributeListener<String>() {

            @Override
            public void onValueChange(List<String> newValue) {
                if (newValue != null && newValue.size() > 0) {
                    System.out.println(newValue.get(0));
                } else {
                    System.out.println("null");
                }

            }

            @Override
            public void onValidityChange(boolean newValidity) {
            }
        });

        try {
            Thread.sleep(5000);
        } catch (InterruptedException e) {
            LOG.warn(e.getMessage());
        }

        service.unbind();
    }

}
