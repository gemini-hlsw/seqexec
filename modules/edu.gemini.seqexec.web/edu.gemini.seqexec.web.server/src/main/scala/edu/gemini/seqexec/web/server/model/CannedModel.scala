package edu.gemini.seqexec.web.server.model

import edu.gemini.seqexec.web.common.{SeqexecQueue, SequenceInQueue, SequenceState}

/**
  * Data for demonstration purposes
  */
object CannedModel {

  val currentQueue = SeqexecQueue(List(
      SequenceInQueue("GS-2016A-Q-0-1", SequenceState.NotRunning, "GPI", None),
      SequenceInQueue("GS-2016A-Q-5-3", SequenceState.Running, "GMOS-S", None),
      SequenceInQueue("GS-2016A-Q-4-1", SequenceState.Error, "Flamingos 2", Some("Error"))
    )
  )

}
