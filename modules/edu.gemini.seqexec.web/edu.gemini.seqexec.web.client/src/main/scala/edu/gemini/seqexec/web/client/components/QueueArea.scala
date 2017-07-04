package edu.gemini.seqexec.web.client.components

import diode.react.ModelProxy
import edu.gemini.seqexec.model.Model.{SequenceState, SequenceView}
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.model.Pages._
import edu.gemini.seqexec.web.client.model.ModelOps._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconAttention, IconCheckmark, IconCircleNotched}
import edu.gemini.seqexec.web.client.semanticui.elements.table.TableHeader
import edu.gemini.seqexec.web.client.services.HtmlConstants.{iconEmpty, nbsp}
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import org.scalajs.dom.html.TableRow

import scalacss.ScalaCssReact._
import scalaz.syntax.show._
import scalaz.syntax.std.option._

object QueueTableBody {
  type SequencesModel = ModelProxy[StatusAndLoadedSequences]

  case class Props(sequences: SequencesModel)

  // Minimum rows to display, pad with empty rows if needed
  val minRows = 5

  def emptyRow(k: String, isLogged: Boolean): VdomTagOf[TableRow] = {
    <.tr(
      ^.key := k, // React requires unique keys
      <.td(
        ^.cls := "collapsing",
        iconEmpty),
      <.td(nbsp),
      <.td(nbsp),
      <.td(nbsp),
      <.td(
        SeqexecStyles.notInMobile,
        nbsp).when(isLogged)
    )
  }

  def showSequence(p: Props, s: SequenceView): Callback =
    // Request to display the selected sequence
    p.sequences.dispatchCB(NavigateTo(InstrumentPage(s.metadata.instrument, s.id.some))) >> p.sequences.dispatchCB(SelectToDisplay(s))

  private val component = ScalaComponent.builder[Props]("QueueTableBody")
    .render_P { p =>
      val (isLogged, sequences) = (p.sequences().isLogged, p.sequences().sequences)
      <.table(
        ^.cls := "ui selectable compact celled table unstackable",
        <.thead(
          <.tr(
            SeqexecStyles.notInMobile,
            TableHeader(TableHeader.Props(collapsing = true),  iconEmpty),
            TableHeader("Obs ID"),
            TableHeader("State"),
            TableHeader("Instrument"),
            TableHeader("Obs. Name").when(isLogged)
          )
        ),
        <.tbody(
          sequences.queue.map(Some.apply).padTo(minRows, None).zipWithIndex.collect {
            case (Some(s), i) =>
              <.tr(
                ^.classSet(
                  "positive" -> (s.status == SequenceState.Completed),
                  "warning"  -> (s.status == SequenceState.Running),
                  "negative" -> s.hasError
                ),
                ^.key := s"item.queue.$i",
                ^.onClick --> showSequence(p, s),
                <.td(
                  ^.cls := "collapsing",
                  s.status match {
                    case SequenceState.Completed                   => IconCheckmark
                    case SequenceState.Running                     => IconCircleNotched.copy(IconCircleNotched.p.copy(loading = true))
                    case SequenceState.Error(_)                    => IconAttention
                    case _                                         => iconEmpty
                  }
                ),
                <.td(
                  ^.cls := "collapsing",
                  s.id
                ),
                <.td(
                  s.status.shows + s.runningStep.map(u => s" ${u._1 + 1}/${u._2}").getOrElse("")
                ),
                <.td(
                  s.metadata.instrument
                ),
                <.td(
                  SeqexecStyles.notInMobile,
                  s.metadata.name
                ).when(isLogged)
              )
            case (_, i) =>
              emptyRow(s"item.queue.$i", isLogged)
          }.toTagMod
        )
      )
    }
    .build

  def apply(p: SequencesModel): Unmounted[Props, Unit, Unit] = component(Props(p))

}

/**
  * Container for the queue table
  */
object QueueTableSection {
  private val sequencesConnect = SeqexecCircuit.connect(SeqexecCircuit.statusAndLoadedSequences)

  private val component = ScalaComponent.builder[Unit]("QueueTableSection")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui segment scroll pane",
        SeqexecStyles.queueListPane,
        sequencesConnect(QueueTableBody.apply)
      )
    ).build

  def apply(): Unmounted[Unit, Unit, Unit] = component()

}

/**
  * Displays the elements on the queue
  */
object QueueArea {

  private val component = ScalaComponent.builder[Unit]("QueueArea")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui raised segments container",
        TextMenuSegment("Night Queue", "key.queue.menu"),
        <.div(
          ^.cls := "ui attached segment",
          <.div(
            ^.cls := "ui grid",
            <.div(
              ^.cls := "stretched row",
              <.div(
                ^.cls := "sixteen wide column",
                QueueTableSection()
              )
            )
          )
        )
      )
    )
    .build

  def apply(): Unmounted[Unit, Unit, Unit] = component()

}
