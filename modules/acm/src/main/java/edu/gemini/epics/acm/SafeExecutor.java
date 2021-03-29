/*
 * Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import org.slf4j.Logger;

public interface SafeExecutor {
    public static ScheduledExecutorService safeExecutor(Integer threadCount, Logger logger, String className) {
        ThreadFactory threadFactory = new ThreadFactory() {

            @Override
            public Thread newThread(Runnable r) {
                final Thread thread = new Thread(r);

                thread.setUncaughtExceptionHandler( new Thread.UncaughtExceptionHandler() {

                    @Override
                    public void uncaughtException(Thread t, Throwable e) {
                        logger.error("Uncaught exception on " + className + " at thread: " + t.getId(), e);
                    }
                });

                return thread;
            }

        };

        return new ScheduledThreadPoolExecutor(2, threadFactory);
    }

}
