package edu.gemini.seqexec.server

import edu.gemini.epics.acm.CaService

import scalaz._
import Scalaz._

/**
 * TcsEpics wraps the non-functional parts of the EPICS ACM library to interact with TCS
 *
 * Created by jluhrs on 10/1/15.
 */

object TcsEpics {
  object m1Guide extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("m1Guide"))
    val state = cs.map(_.getString("state"))
    def setState(v: String): SeqAction[Unit] = setParameter(state, v)
  }

  object m2Guide extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("m2Guide"))
    val state = cs.map(_.getString("state"))
    def setState(v: String): SeqAction[Unit] = setParameter[String](state, v)
  }

  object mountGuide extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("mountGuide"))

    val source = cs.map(_.getString("source"))
    def setSource(v: String): SeqAction[Unit] = setParameter(source, v)

    val p1weight = cs.map(_.getDouble("p1weight"))
    def setP1Weight(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](p1weight, v)

    val p2weight = cs.map(_.getDouble("p2weight"))
    def setP2Weight(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](p2weight, v)

    val mode = cs.map(_.getString("mode"))
    def setMode(v: String): SeqAction[Unit] = setParameter(mode, v)
  }

  object offsetA extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("offsetPoA1"))

    val x = cs.map(_.getDouble("x"))
    def setX(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](x, v)

    val y = cs.map(_.getDouble("y"))
    def setY(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](y, v)
  }

  object offsetB extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("offsetPoB1"))

    val x = cs.map(_.getDouble("x"))
    def setX(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](x, v)

    val y = cs.map(_.getDouble("y"))
    def setY(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](y, v)
  }

  object offsetC extends EpicsCommand {
    override val cs = Option(CaService.getInstance().getCommandSender("offsetPoC1"))

    val x = cs.map(_.getDouble("x"))
    def setX(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](x, v)

    val y = cs.map(_.getDouble("y"))
    def setY(v: Double): SeqAction[Unit] = setParameter[java.lang.Double](y, v)
  }

}
