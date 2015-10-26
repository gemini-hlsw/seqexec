package edu.gemini.seqexec.server.osgi

import edu.gemini.seqexec.server.{SeqexecFailure, TcsEpicsInitializer}
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

    TcsEpicsInitializer.init.leftMap {
      case SeqexecFailure.SeqexecException(ex) => throw ex
      case c: SeqexecFailure => throw new Exception(SeqexecFailure.explain(c))
    }

    reg = Some(ctx.registerService(classOf[Commands], Commands(), dict))
  }

  override def stop(ctx: BundleContext): Unit = {
    println("edu.gemini.seqexec.client stop")

    TcsEpicsInitializer.cleanup

    reg.foreach(_.unregister())
    reg = None
  }
}
