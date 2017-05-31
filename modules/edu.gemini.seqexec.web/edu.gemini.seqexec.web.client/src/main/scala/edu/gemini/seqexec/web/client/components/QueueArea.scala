package edu.gemini.seqexec.web.client.components

import diode.react._
import edu.gemini.seqexec.model.Model.{SequenceState, SequenceView}
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.model.ModelOps._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconAttention, IconCheckmark, IconCircleNotched}
import edu.gemini.seqexec.web.client.semanticui.elements.table.TableHeader
import edu.gemini.seqexec.web.client.services.HtmlConstants.{iconEmpty, nbsp}
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import org.scalajs.dom.html.TableRow

import scala.scalajs.js
import scalacss.ScalaCssReact._
import scalaz.syntax.show._

object QueueTableBody {
  case class Props(sequences: ModelProxy[SeqexecAppRootModel.LoadedSequences])

  // Minimum rows to display, pad with empty rows if needed
  val minRows = 5

  def emptyRow(k: String): VdomTagOf[TableRow] = {
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
        nbsp)
    )
  }

  def showSequence(p: Props,s: SequenceView): Callback =
    // Request to display the selected sequence
    p.sequences.dispatchCB(SelectToDisplay(s))

  private val component = ScalaComponent.builder[Props]("QueueTableBody")
    .render_P( p =>
      <.tbody(
        // Render after data arrives
        p.sequences().queue.map(Some.apply).padTo(minRows, None).zipWithIndex.collect {
            case (Some(s), i) =>
              <.tr(
                ^.classSet(
                  "positive" -> (s.status == SequenceState.Completed),
                  "warning"  -> (s.status == SequenceState.Running),
                  "negative" -> s.hasError
                  //"negative" -> (s.status == SequenceState.Abort)
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
                )
              )
            case (_, i) =>
              emptyRow(s"item.queue.$i")
          }.toTagMod
      )
    )
    .build

  def apply(p: ModelProxy[SeqexecAppRootModel.LoadedSequences]) = component(Props(p))

}

/**
  * Container for the queue table
  */
object QueueTableSection {
  private val queueConnect = SeqexecCircuit.connect(_.sequences, "key.queue": js.Any)

  private val component = ScalaComponent.builder[Unit]("QueueTableSection")
    .stateless
    .render_P(_ =>
      <.div(
        ^.cls := "ui segment scroll pane",
        SeqexecStyles.queueListPane,
        <.table(
          ^.cls := "ui selectable compact celled table unstackable",
          <.thead(
            <.tr(
              SeqexecStyles.notInMobile,
              TableHeader(TableHeader.Props(collapsing = true),  iconEmpty),
              TableHeader("Obs ID"),
              TableHeader("State"),
              TableHeader("Instrument"),
              TableHeader("Obs. Name")
            )
          ),
          queueConnect(QueueTableBody(_))
        )
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
