package edu.gemini.seqexec.web.client.components.sequence

import diode.react.ReactPot._
import edu.gemini.seqexec.web.client.components.{SeqexecStyles, TabularMenu, TextMenuSegment}
import edu.gemini.seqexec.web.client.components.TabularMenu.TabItem
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.elements.divider.Divider
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconCaretRight, IconInbox, IconPause, IconPlay}
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconAttention, IconCheckmark, IconCircleNotched, IconStop}
import edu.gemini.seqexec.web.client.semanticui.elements.message.IconMessage
import edu.gemini.seqexec.web.common.{Sequence, SequenceState, StepState}
import edu.gemini.seqexec.web.client.services.HtmlConstants.iconEmpty
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactElement}
import scalacss.ScalaCssReact._
import scalaz.syntax.show._

/**
  * Container for a table with the steps
  */
object SequenceStepsTableContainer {
  case class Props(s: Sequence)

  def requestRun(s: Sequence): Callback = Callback {SeqexecCircuit.dispatch(RequestRun(s))}

  def requestPause(s: Sequence): Callback = Callback.log("Request pause")

  def requestStop(s: Sequence): Callback = Callback {SeqexecCircuit.dispatch(RequestStop(s))}

  val component = ReactComponentB[Props]("HeadersSideBar")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui raised secondary segment",
        <.div(
          ^.cls := "row",
          p.s.state == SequenceState.Abort ?= <.h3(
            ^.cls := "ui red header",
            "Sequence aborted"),
          p.s.state == SequenceState.Completed ?= <.h3(
            ^.cls := "ui green header",
            "Sequence completed"),
          p.s.state == SequenceState.NotRunning ?= Button(Button.Props(icon = Some(IconPlay), labeled = true, onClick = requestRun(p.s)), "Run"),
          p.s.state == SequenceState.Running ?= Button(Button.Props(icon = Some(IconPause), labeled = true, disabled = true, onClick = requestPause(p.s)), "Pause"),
          p.s.state == SequenceState.Running ?= Button(Button.Props(icon = Some(IconStop), labeled = true, onClick = requestStop(p.s)), "Stop")
        ),
        Divider(),
        <.div(
          ^.cls := "row",
          <.table(
            ^.cls := "ui selectable compact celled table",
            <.thead(
              <.tr(
                <.th(
                  ^.cls := "collapsing",
                  iconEmpty
                ),
                <.th(
                  ^.cls := "collapsing",
                  "Step"
                ),
                <.th(
                  ^.cls := "six wide",
                  "State"
                ),
                <.th(
                  ^.cls := "ten wide",
                  "File"
                ),
                <.th(
                  ^.cls := "collapsing",
                  "Config"
                )
              )
            ),
            <.tbody(
              p.s.steps.steps.map( s =>
                <.tr(
                  ^.classSet(
                    "positive" -> (s.state == StepState.Done),
                    "warning"  -> (s.state == StepState.Running),
                    "negative" -> (s.state == StepState.Error),
                    "negative" -> (s.state == StepState.Abort)
                  ),
                  <.td(
                    s.state match {
                      case StepState.Done    => IconCheckmark
                      case StepState.Running => IconCircleNotched.copy(IconCircleNotched.p.copy(loading = true))
                      case StepState.Error   => IconAttention
                      case _                 => iconEmpty
                    }
                  ),
                  <.td(s.id + 1),
                  <.td(s.state.shows),
                  <.td(s.file.getOrElse(""): String),
                  <.td(
                    ^.cls := "collapsing right aligned",
                    IconCaretRight
                  )
                )
              )
            )
          )
        )
      )
    )
    .build

  def apply(s: Sequence) = component(Props(s))
}

/**
  * Content of a single tab with a sequence
  */
object SequenceTabContent {
  case class Props(isActive: Boolean, st: SequenceTab)

  val component = ReactComponentB[Props]("SequenceTabContent")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui bottom attached tab segment",
        ^.classSet(
          "active" -> p.isActive
        ),
        dataTab := p.st.instrument,
        p.st.sequence().render(s => SeqexecCircuit.connect(SeqexecCircuit.sequenceReader(s.id))(u => u().map(SequenceStepsTableContainer(_)).getOrElse(<.div(): ReactElement))),
        p.st.sequence().renderEmpty(IconMessage(IconMessage.Props(IconInbox, Some("No sequence loaded"), IconMessage.Style.Warning)))
      )
    )
    .build

  def apply(p: Props) = component(p)
}

/**
  * Contains all the tabs for the sequences available in parallel
  */
object SequenceTabs {
  case class Props(sequences: SequencesOnDisplay)

  def sequencesTabs(d: SequencesOnDisplay) = d.instrumentSequences.map(a => TabItem(a.instrument, isActive = a == d.instrumentSequences.focus, a.instrument))
  def tabContents(d: SequencesOnDisplay) = d.instrumentSequences.map(a => SequenceTabContent.Props(isActive = a == d.instrumentSequences.focus, a)).toStream

  val component = ReactComponentB[Props]("SequenceTabs")
    .stateless
    .render_P( p =>
      <.div(
        ^.cls := "ui bottom attached segment",
        <.div(
          ^.cls := "ui two column vertically divided grid",
          <.div(
            ^.cls := "row",
            SeqexecStyles.rowNoPadding,
            <.div(
              ^.cls := "four wide column computer tablet only",
              HeadersSideBar()
            ),
            // Computer/tablet view
            <.div(
              ^.cls := "twelve wide column computer tablet only",
              TabularMenu(sequencesTabs(p.sequences).toStream.toList),
              tabContents(p.sequences).map(SequenceTabContent(_))
            ),
            // Mobile view
            <.div(
              ^.cls := "sixteen wide column",
              SeqexecStyles.onlyMobile,
              TabularMenu(sequencesTabs(p.sequences).toStream.toList),
              tabContents(p.sequences).map(SequenceTabContent(_))
            )
          ),
          <.div(
            ^.cls := "row computer only",
            <.div(
              ^.cls := "sixteen wide column",
              SeqexecCircuit.connect(_.globalLog)(LogArea(_))
            )
          )
        )
      )
    )
    .build

  def apply(sequences: SequencesOnDisplay) = component(Props(sequences))
}

object SequenceArea {
  val component = ReactComponentB[Unit]("QueueTableSection")
    .stateless
    .render( _ =>
      <.div(
        ^.cls := "ui raised segments container",
        TextMenuSegment("Running Sequences"),
        SeqexecCircuit.connect(_.sequencesOnDisplay)(p => SequenceTabs(p()))
      )
    ).build

  def apply() = component()
}
