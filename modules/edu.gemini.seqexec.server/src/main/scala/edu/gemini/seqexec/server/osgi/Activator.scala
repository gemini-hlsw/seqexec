package edu.gemini.seqexec.server.osgi

import org.osgi.framework.{BundleActivator, BundleContext, ServiceRegistration}

object Activator {
  val CommandScope = "osgi.command.scope"
  val CommandFunction = "osgi.command.function"
}

class Activator extends BundleActivator {

  import Activator._

  private var reg: Option[ServiceRegistration[Commands]] = None

  override def start(ctx: BundleContext): Unit = {
    println("edu.gemini.seqexec.client start")

    val dict = new java.util.Hashtable[String, Object]()
    dict.put(CommandScope, "seq")
    dict.put(CommandFunction, Array("seq"))
    reg = Some(ctx.registerService(classOf[Commands], Commands(), dict))
  }

  override def stop(ctx: BundleContext): Unit = {
    println("edu.gemini.seqexec.client stop")
    reg.foreach(_.unregister())
    reg = None
  }
}
