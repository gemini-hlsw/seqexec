package edu.gemini.seqexec.shared.osgi

import java.util

import edu.gemini.pot.spdb.IDBDatabaseService
import edu.gemini.seqexec.shared.{SeqExecService, SeqExecServer}
import org.osgi.framework.{ServiceRegistration, BundleContext, BundleActivator}

import edu.gemini.util.osgi.Tracker._
import org.osgi.util.tracker.ServiceTracker


object Activator {
  val PublishTrpc = "trpc"
}

class Activator extends BundleActivator {
  private var tracker: Option[ServiceTracker[_,_]] = None

  override def start(ctx: BundleContext): Unit = {
    import Activator._

    tracker = Some(track[IDBDatabaseService, ServiceRegistration[_]](ctx) { (odb) =>
      val seqServer = new SeqExecServer(odb)

      val props = new util.Hashtable[String, Object]()
      props.put(PublishTrpc, "true")
      ctx.registerService(classOf[SeqExecService], seqServer, props)
    } { _.unregister() })

    tracker.foreach(_.open())
  }

  override  def stop(ctx: BundleContext): Unit = {
    tracker.foreach(_.close())
    tracker = None
  }
}
