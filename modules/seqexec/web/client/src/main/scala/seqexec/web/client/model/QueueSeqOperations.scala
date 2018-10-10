// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import monocle.macros.Lenses

sealed trait RemoveSeqQueue extends Product with Serializable
object RemoveSeqQueue {
  case object RemoveSeqQueueInFlight extends RemoveSeqQueue
  case object RemoveSeqQueueIdle extends RemoveSeqQueue

  implicit val eq: Eq[RemoveSeqQueue] =
    Eq.fromUniversalEquals

}

/**
  * Hold transient states while excuting an operation on a queue element
  */
@Lenses
final case class QueueSeqOperations(removeSeqQueue: RemoveSeqQueue)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object QueueSeqOperations {
  implicit val eq: Eq[QueueSeqOperations] =
    Eq.by(x => (x.removeSeqQueue))

  val Default: QueueSeqOperations =
    QueueSeqOperations(RemoveSeqQueue.RemoveSeqQueueIdle)
}
