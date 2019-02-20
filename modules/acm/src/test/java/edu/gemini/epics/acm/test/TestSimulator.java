/*
 * Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm.test;

import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import edu.gemini.aspen.giapi.commands.Command;
import edu.gemini.aspen.giapi.commands.CommandSender;
import edu.gemini.aspen.giapi.commands.CompletionListener;
import edu.gemini.aspen.giapi.commands.HandlerResponse;
import edu.gemini.cas.impl.ChannelAccessServerImpl;
import edu.gemini.epics.api.Channel;
import edu.gemini.gmp.commands.records.CommandRecordsBuilder;
import edu.gemini.gmp.top.TopImpl;
import gov.aps.jca.CAException;
import gov.aps.jca.TimeoutException;

final class TestSimulator {

    private static final Logger LOG = LoggerFactory.getLogger(TestSimulator.class.getName());

    static final String INTEGER_STATUS = "intVal";
    static final String STRING_STATUS = "strVal";
    static final String DOUBLE_STATUS = "dblVal";
    static final String FLOAT_STATUS = "fltVal";
    
    static final String ERROR_MSG = "Test Error";

    private static final String CONFIG_FILE = "/TestSim.xml";

    private final CommandRecordsBuilder sys;
    private ChannelAccessServerImpl server;
    private ScheduledThreadPoolExecutor executor;
    private Channel<Integer> intChannel;
    private Channel<String> strChannel;
    private Channel<Double> dblChannel;
    private Channel<Float> fltChannel;
    private ScheduledFuture<?> ticker;
    private final String epicsTop;

    public TestSimulator(String epicsTop) {
        this.epicsTop = epicsTop;
        server = new ChannelAccessServerImpl();
        executor = new ScheduledThreadPoolExecutor(5);

        sys = new CommandRecordsBuilder(server, new CommandSender() {

            @Override
            public HandlerResponse sendCommand(final Command command,
                    final CompletionListener listener) {
                switch (command.getActivity()) {
                case PRESET:
                case CANCEL:
                    return HandlerResponse.ACCEPTED;
                case PRESET_START:
                case START:
                    if (command.getSequenceCommand().getName().equals("test")) {
                        executor.schedule(new Runnable() {

                            @Override
                            public void run() {
                                listener.onHandlerResponse(
                                        HandlerResponse.COMPLETED, command);
                            }
                        }, 1, TimeUnit.SECONDS);
                    } else if (command.getSequenceCommand().getName()
                            .equals("reboot")) {
                        executor.schedule(new Runnable() {

                            @Override
                            public void run() {
                                listener.onHandlerResponse(HandlerResponse
                                        .createError(ERROR_MSG), command);
                            }
                        }, 1, TimeUnit.SECONDS);
                    }
                    return HandlerResponse.STARTED;
                default:
                    return HandlerResponse
                            .createError("Invalid command activity");
                }
            }

            @Override
            public HandlerResponse sendCommand(Command command,
                    CompletionListener listener, long timeout) {

                return sendCommand(command, listener);
            }

        }, new TopImpl(epicsTop, epicsTop), TestSimulator.class.getResource(
                CONFIG_FILE).getPath());
    }

    public void start() {
        try {
            server.start();
        } catch (CAException e) {
            LOG.warn(e.getMessage());
        }
        try {
            intChannel = server.createChannel(epicsTop + ":" + INTEGER_STATUS,
                    0);
            strChannel = server.createChannel(epicsTop + ":" + STRING_STATUS,
                    "0");
            dblChannel = server.createChannel(epicsTop + ":" + DOUBLE_STATUS,
                    0.0);
            fltChannel = server.createChannel(epicsTop + ":" + FLOAT_STATUS,
                    0.0f);
        } catch (CAException e) {
            LOG.warn(e.getMessage());
        }

        ticker = executor.scheduleAtFixedRate(new Runnable() {
            int count = 0;

            @Override
            public void run() {
                count++;
                try {
                    intChannel.setValue(count);
                    strChannel.setValue(Integer.toString(count));
                    dblChannel.setValue((double) count);
                    fltChannel.setValue((float) count);
                } catch (CAException | TimeoutException e) {
                    LOG.warn(e.getMessage());
                }
            }
        }, 1, 1, TimeUnit.SECONDS);

        sys.start();
    }

    public void stop() {
        sys.stop();

        ticker.cancel(false);
        try {
            executor.awaitTermination(10, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            LOG.warn(e.getMessage());
        }
        executor.shutdown();
        executor = null;
        
        server.destroyChannel(intChannel);
        server.destroyChannel(strChannel);
        server.destroyChannel(dblChannel);
        server.destroyChannel(fltChannel);

        try {
            server.stop();
        } catch (CAException e) {
            LOG.warn(e.getMessage());
        }
        server = null;
    }

    public static void main(String[] args) {
        final TestSimulator theSys = new TestSimulator("dummy");
        theSys.start();

        Runtime.getRuntime().addShutdownHook(new Thread() {
            @Override
            public void run() {
                System.out.println("Stopping simulator.");
                theSys.stop();
            }
        });
    }

}
