package edu.gemini.seqexec.web.client.model

import edu.gemini.seqexec.model.Model.SeqexecEvent.{ConnectionOpenEvent, SequenceLoaded}
import edu.gemini.seqexec.model.Model.SequenceView
import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.web.client.model.SeqexecCircuit.zoomRW
import edu.gemini.seqexec.web.common.ArbitrariesWebCommon
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class WebSocketsEventsHandlerSpec extends FlatSpec with Matchers with PropertyChecks with ArbitrariesWebCommon {
  import edu.gemini.seqexec.model.SharedModelArbitraries._

  "WebSocketsEventsHandler" should "accept connection open events anonymously" in {
    val handler = new WebSocketEventsHandler(zoomRW(m => (m.sequences, m.webSocketLog, m.user))((m, v) => m.copy(sequences = v._1, webSocketLog = v._2, user = v._3)))
    val result = handler.handle(NewSeqexecEvent(ConnectionOpenEvent(None)))
    // No user set
    result.newModelOpt.flatMap(_.user) shouldBe None
  }
  it should "set the user if the response contains one" in {
    val handler = new WebSocketEventsHandler(zoomRW(m => (m.sequences, m.webSocketLog, m.user))((m, v) => m.copy(sequences = v._1, webSocketLog = v._2, user = v._3)))
    val user = UserDetails("user", "User name")
    val result = handler.handle(NewSeqexecEvent(ConnectionOpenEvent(Some(user))))
    // No user set
    result.newModelOpt.flatMap(_.user) shouldBe Some(user)
  }
  it should "accept a loaded Sequence" in {
    forAll { (sequence: SequenceView) =>
      val handler = new WebSocketEventsHandler(zoomRW(m => (m.sequences, m.webSocketLog, m.user))((m, v) => m.copy(sequences = v._1, webSocketLog = v._2, user = v._3)))
      val result = handler.handle(NewSeqexecEvent(SequenceLoaded(List(sequence))))
      // No user set
      result.newModelOpt.exists(_.sequences.contains(sequence)) shouldBe true
    }
  }
  it should "accept many loaded Sequences" in {
    forAll { (sequences: List[SequenceView]) =>
      val handler = new WebSocketEventsHandler(zoomRW(m => (m.sequences, m.webSocketLog, m.user))((m, v) => m.copy(sequences = v._1, webSocketLog = v._2, user = v._3)))
      val result = handler.handle(NewSeqexecEvent(SequenceLoaded(sequences)))
      // No user set
      result.newModelOpt.exists(_.sequences === sequences) shouldBe true
    }
  }

}
