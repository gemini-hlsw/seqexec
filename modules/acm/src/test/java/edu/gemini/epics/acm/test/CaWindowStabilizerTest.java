/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm.test;

import edu.gemini.epics.acm.CaWindowStabilizer;
import org.junit.Test;

import java.time.Duration;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;

public class CaWindowStabilizerTest {

    private final ScheduledExecutorService executor = new ScheduledThreadPoolExecutor(2);

    @Test
    public void testFilteredConstantValue() {
        Duration settleTime = Duration.ofMillis(100);

        DummyAttribute<Integer> attr = new DummyAttribute<Integer>("foo", "dummy:foo");

        CaWindowStabilizer<Integer> filteredVal = new CaWindowStabilizer<Integer>(attr, settleTime, executor);

        attr.setValue(1);

        filteredVal.restart();
        try {
            Thread.sleep(settleTime.toMillis() * 2);
        } catch(Exception ignored) {}

        assert(filteredVal.valid() && filteredVal.value() == 1);
    }

    @Test
    public void testZeroSettleTime() {
        Duration settleTime = Duration.ofMillis(0);

        DummyAttribute<Integer> attr = new DummyAttribute<Integer>("foo", "dummy:foo");

        CaWindowStabilizer<Integer> filteredVal = new CaWindowStabilizer<Integer>(attr, settleTime, executor);

        attr.setValue(1);

        filteredVal.restart();
        try {
            Thread.sleep(100);
        } catch(Exception ignored) {}

        assert(filteredVal.valid() && filteredVal.value() == 1);
    }

}
