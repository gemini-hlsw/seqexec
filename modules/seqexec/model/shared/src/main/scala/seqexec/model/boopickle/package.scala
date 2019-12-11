// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import _root_.boopickle.Default.Pickler
import _root_.boopickle.Default.Pickle
import _root_.boopickle.Default.PickleState
import _root_.boopickle.Default.UnpickleState
import monocle.Prism
import seqexec.model.events.SeqexecEvent

package boopickle {

  trait BooPicklerSyntax {
    implicit class PicklerPrismOps[A, B <: AnyRef](p: Prism[A, B])(implicit PA: Pickler[A]) {
      def toPickler: Pickler[B] =
        new Pickler[B] {
          override def pickle(obj: B)(implicit state: PickleState): Unit = {
            state.identityRefFor(obj) match {
              case Some(idx) =>
                state.enc.writeInt(-idx)
                ()
              case None =>
                // We need a marker
                state.enc.writeInt(0)
                // Use the prism to get a representatative for the A and encode it
                PA.pickle(p.reverseGet(obj))
                state.addIdentityRef(obj)
            }
          }

          override def unpickle(implicit state: UnpickleState): B = {
            state.dec.readInt match {
              case idx if idx < 0 =>
                state.identityFor[B](-idx)
              case _ =>
                val a = PA.unpickle(state)
                val c = p.getOption(a).getOrElse(throw new IllegalArgumentException("Invalid coding for prism"))
                state.addIdentityRef(c)
                c
            }
          }
        }
    }
  }

}

package object boopickle extends ModelBooPicklers {

  /**
    * In most cases http4s will use the limit of a byte buffer but not for websockets
    * This method trims the binary array to be sent on the WS channel
    */
  def trimmedArray(e: SeqexecEvent): Array[Byte] = {
    val byteBuffer = Pickle.intoBytes(e)
    val bytes      = new Array[Byte](byteBuffer.limit())
    byteBuffer.get(bytes, 0, byteBuffer.limit)
    bytes
  }

}
