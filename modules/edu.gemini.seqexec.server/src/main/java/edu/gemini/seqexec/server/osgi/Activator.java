package edu.gemini.seqexec.server.osgi;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
/**
 * Created by jluhrs on 4/17/15.
 */
public class Activator implements BundleActivator {
    @Override
    public void start(BundleContext bundleContext) throws Exception {
        System.out.println("Hello World!");
    }

    @Override
    public void stop(BundleContext bundleContext) throws Exception {

    }
}
