package edu.gemini.seqexec.web.client.model

import diode.data.{Empty, Pot, PotAction}
import diode.react.ReactConnector
import diode.util.RunAfterJS
import diode._
import edu.gemini.seqexec.web.client.model.SeqexecCircuit.SearchResults
import edu.gemini.seqexec.web.client.services.SeqexecWebClient
import edu.gemini.seqexec.web.common.{SeqexecQueue, Sequence}
import org.scalajs.dom.{Event, WebSocket}

import scala.concurrent.duration._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// Action Handlers
/**
  * Handles actions related to the queue like loading and adding new elements
  */
class QueueHandler[M](modelRW: ModelRW[M, Pot[SeqexecQueue]]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle = {
    case action: UpdatedQueue =>
      // Request loading the queue with ajax
      val loadEffect = action.effect(SeqexecWebClient.readQueue())(identity)
      action.handleWith(this, loadEffect)(PotAction.handler(250.milli))
    case AddToQueue(s) =>
      // Append to the current queue if not in the queue already
      val u = value.map(u => u.copy((s :: u.queue.filter(_.id != s.id)).reverse))
      updated(u)
  }
}

/**
  * Handles actions related to search
  */
class SearchHandler[M](modelRW: ModelRW[M, Pot[SeqexecCircuit.SearchResults]]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle = {
    case action: SearchSequence =>
      // Request loading the queue with ajax
      val loadEffect = action.effect(SeqexecWebClient.read(action.criteria))(identity)
      action.handleWith(this, loadEffect)(PotAction.handler(250.milli))
    case RemoveFromSearch(s) =>
      updated(value.map(_.filterNot(_ == s)))
  }
}

/**
  * Handles actions related to the search area, used to open/close the area
  */
class SearchAreaHandler[M](modelRW: ModelRW[M, SectionVisibilityState]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle = {
    case OpenSearchArea  =>
      updated(SectionOpen)
    case CloseSearchArea =>
      updated(SectionClosed)
  }
}

/**
  * Handles actions related to the development console
  */
class DevConsoleHandler[M](modelRW: ModelRW[M, SectionVisibilityState]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle = {
    case ToggleDevConsole if value == SectionOpen   =>
      updated(SectionClosed)
    case ToggleDevConsole if value == SectionClosed =>
      updated(SectionOpen)
  }
}

/**
  * Handles actions related to the changing the selection of the displayed sequence
  */
class SequenceDisplayHandler[M](modelRW: ModelRW[M, SequencesOnDisplay]) extends ActionHandler(modelRW) {
  implicit val runner = new RunAfterJS

  override def handle = {
    case SelectToDisplay(s) =>
      updated(value.sequenceForInstrument(s))
  }
}

/**
  * Generates Eq comparisons for Pot[A], it is useful for state indicators
  */
object PotEq {
  def potStateEq[A]: FastEq[Pot[A]] = new FastEq[Pot[A]] {
    override def eqv(a: Pot[A], b: Pot[A]): Boolean = a.state == b.state
  }

  val seqexecQueueEq = potStateEq[SeqexecQueue]
  val searchResultsEq = potStateEq[SearchResults]
}

/**
  * Contains the model for Diode
  */
object SeqexecCircuit extends Circuit[SeqexecAppRootModel] with ReactConnector[SeqexecAppRootModel] {
  type SearchResults = List[Sequence]

  val webSocket = {

    def onopen(e: Event): Unit = {
      println("Open")
    }

    val ws = new WebSocket("ws://localhost:9090/api/seqexec/events")
    ws.onopen = onopen _
    Some(ws)
  }

  val queueHandler           = new QueueHandler(zoomRW(_.queue)((m, v) => m.copy(queue = v)))
  val searchHandler          = new SearchHandler(zoomRW(_.searchResults)((m, v) => m.copy(searchResults = v)))
  val searchAreaHandler      = new SearchAreaHandler(zoomRW(_.searchAreaState)((m, v) => m.copy(searchAreaState = v)))
  val devConsoleHandler      = new DevConsoleHandler(zoomRW(_.devConsoleState)((m, v) => m.copy(devConsoleState = v)))
  val sequenceDisplayHandler = new SequenceDisplayHandler(zoomRW(_.sequencesOnDisplay)((m, v) => m.copy(sequencesOnDisplay = v)))

  override protected def initialModel = SeqexecAppRootModel.initial

  override protected def actionHandler = composeHandlers(queueHandler, searchHandler, searchAreaHandler, devConsoleHandler, sequenceDisplayHandler)
}
