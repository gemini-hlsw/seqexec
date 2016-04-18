package edu.gemini.seqexec.web.client.components.sequence

import edu.gemini.seqexec.web.client.components.{TabularMenu, TextMenuSegment}
import edu.gemini.seqexec.web.client.components.TabularMenu.TabItem
import edu.gemini.seqexec.web.client.model.{SeqexecCircuit, SequenceTab, SequencesOnDisplay}
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconCaretRight, IconInbox}
import edu.gemini.seqexec.web.client.semanticui.elements.message.IconMessage
import edu.gemini.seqexec.web.common.Sequence
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, ReactElement}

/**
  * Container for a table with the steps
  */
object SequenceStepsTableContainer {
  case class Props(s: Sequence)

  val component = ReactComponentB[Props]("HeadersSideBar")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui raised secondary segment",
        <.div(
          ^.cls := "row",
          Button("Run"),
          Button("Pause")
        ),
        <.div(
          ^.cls := "ui divider"
        ),
        <.div(
          ^.cls := "row",
          <.table(
            ^.cls := "ui selectable compact celled table",
            <.thead(
              <.tr(
                <.th("Step"),
                <.th("State"),
                <.th("Config")
              )
            ),
            <.tbody(
              p.s.steps.steps.map( s =>
                <.tr(
                  <.td(s.id + 1),
                  <.td("Not Done"),
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
        ^.cls := "ui bottom attached active tab segment",
        ^.classSet(
          "active" -> p.isActive
        ),
        dataTab := p.st.instrument,
        p.isActive ?=
          <.div(
            ^.cls := "ui grid",
            <.div(
              ^.cls := "row",
              <.div(
                ^.cls := "four wide column tablet computer only",
                HeadersSideBar()
              ),
              <.div(
                ^.cls := "twelve wide computer twelve wide tablet sixteen column",
                p.st.sequence.fold(IconMessage(IconMessage.Props(IconInbox, Some("No sequence loaded"), IconMessage.Style.Warning)): ReactElement)(SequenceStepsTableContainer(_))
              )
            ),
            <.div(
              ^.cls := "row computer only",
              <.div(
                ^.cls := "sixteen wide column",
                LogArea()
              )
            )
        )
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
        TabularMenu(sequencesTabs(p.sequences).toStream.toList),
        tabContents(p.sequences).map(SequenceTabContent(_))
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
