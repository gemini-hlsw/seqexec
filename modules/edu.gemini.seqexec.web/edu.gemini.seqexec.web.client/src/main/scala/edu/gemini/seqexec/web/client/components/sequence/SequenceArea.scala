package edu.gemini.seqexec.web.client.components.sequence

import edu.gemini.seqexec.web.client.components.{TabularMenu, TextMenuSegment}
import edu.gemini.seqexec.web.client.components.TabularMenu.TabItem
import edu.gemini.seqexec.web.client.model.{SeqexecCircuit, SequenceTab, SequencesOnDisplay}
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconCaretRight, IconInbox}
import edu.gemini.seqexec.web.client.semanticui.elements.message.IconMessage
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, ReactElement}

object SequenceContainer {
  val component = ReactComponentB[Unit]("HeadersSideBar")
    .stateless
    .render(_ =>
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
              <.tr(
                <.td("1"),
                <.td("Done"),
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
    .build

  def apply() = component()
}

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
                p.st.sequence.fold(IconMessage(IconMessage.Props(IconInbox, Some("No sequence loaded"), IconMessage.Style.Warning)): ReactElement)(_ => SequenceContainer())
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
