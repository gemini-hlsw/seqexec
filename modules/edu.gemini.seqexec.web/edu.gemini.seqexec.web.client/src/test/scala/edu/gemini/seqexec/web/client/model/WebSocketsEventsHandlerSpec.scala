package edu.gemini.seqexec.web.client.model

import edu.gemini.seqexec.model.Model.SeqexecEvent.{ConnectionOpenEvent, SequenceLoaded}
import edu.gemini.seqexec.model.Model.{SequenceView, SequencesQueue}
import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.web.client.model.SeqexecCircuit.zoomRW
import edu.gemini.seqexec.web.common.ArbitrariesWebCommon
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

import scalaz._
import Scalaz._

class WebSocketsEventsHandlerSpec extends FlatSpec with Matchers {

  "WebSocketsEventsHandler" should "accept connection open events anonymously" in {
    val handler = new WebSocketEventsHandler(zoomRW(m => (m.sequences, m.webSocketLog, m.user))((m, v) => m.copy(sequences = v._1, webSocketLog = v._2, user = v._3)))
    val result = handler.handle(ServerMessage(ConnectionOpenEvent(None)))
    // No user set
    result.newModelOpt.flatMap(_.user) shouldBe None
  }
  it should "set the user if the response contains one" in {
    val handler = new WebSocketEventsHandler(zoomRW(m => (m.sequences, m.webSocketLog, m.user))((m, v) => m.copy(sequences = v._1, webSocketLog = v._2, user = v._3)))
    val user = UserDetails("user", "User name")
    val result = handler.handle(ServerMessage(ConnectionOpenEvent(Some(user))))
    // No user set
    result.newModelOpt.flatMap(_.user) shouldBe Some(user)
  }
  it should "accept a loaded SequencesQueue" in {
    val sequences = SequencesQueue(List.empty[SequenceView])
    val handler = new WebSocketEventsHandler(zoomRW(m => (m.sequences, m.webSocketLog, m.user))((m, v) => m.copy(sequences = v._1, webSocketLog = v._2, user = v._3)))
    val result = handler.handle(ServerMessage(SequenceLoaded(sequences)))
    // No user set
    result.newModelOpt.exists(_.sequences === sequences) shouldBe true
  }

}
