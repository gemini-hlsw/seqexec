package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.web.client.model.SeqexecCircuit
import edu.gemini.seqexec.web.client.services.SeqexecWebClient
import edu.gemini.seqexec.web.common.Sequence
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB}
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.ext.AjaxException

import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Top level UI component
  */
object SeqexecUI {

  case class State(searching: Boolean, errorMessage: List[String], sequence: Option[Sequence], runningSequence: List[String])

  class Backend($: BackendScope[Unit, State]) {

    def readRemote(id: String): Callback = {
      def handleSearchError: String => Callback = e => {
        $.modState(s => s.copy(searching = false, List(e), None))
      }

      Callback.future(
        SeqexecWebClient.read(id).collect {
          case u: Sequence => $.modState(s => s.copy(searching = false, Nil, Some(u)))
        }.recover {
          case e: AjaxException if e.xhr.status == 404 => handleSearchError(s"Sequence '$id' not found")
          case e                                       => handleSearchError(e.getMessage)
        }
      )
    }

    def searchStart: String => Callback = s => {
      $.modState(s => s.copy(searching = true)) >> readRemote(s)
    }

    def runSequence(seq: Sequence):Callback =
      $.modState(s => s.copy(runningSequence = s.runningSequence :+ seq.id))

    def render(s:State) = {
      <.div(
        NavBar(),
        SeqexecCircuit.connect(_.queue)(proxy => QueueArea(proxy))
      )
    }
  }

  val component = ReactComponentB[Unit]("Seqexec")
    .initialState_P(_ => State(searching = false, Nil, None, Nil))
    .renderBackend[Backend]
    .buildU

  def apply() = component()
}
