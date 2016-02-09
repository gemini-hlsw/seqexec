package edu.gemini.seqexec.server.osgi

import edu.gemini.epics.acm.CaService
import edu.gemini.seqexec.server.{Flamingos2Epics, SeqexecFailure, TcsEpics}
import org.osgi.framework.{BundleActivator, BundleContext, ServiceRegistration}
import scalaz._
import Scalaz._

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

    Option(CaService.getInstance()) match {
      case None => throw new Exception("Unable to start EPICS service.")
      case Some(s) => {
        List(TcsEpics, Flamingos2Epics).traverseU(_.init(s)).leftMap {
          case SeqexecFailure.SeqexecException(ex) => throw ex
          case c: SeqexecFailure => throw new Exception(SeqexecFailure.explain(c))
        }
      }
    }

    reg = Some(ctx.registerService(classOf[Commands], Commands(), dict))
  }

  override def stop(ctx: BundleContext): Unit = {
    println("edu.gemini.seqexec.client stop")

    CaService.getInstance().unbind()

    reg.foreach(_.unregister())
    reg = None
  }
}
