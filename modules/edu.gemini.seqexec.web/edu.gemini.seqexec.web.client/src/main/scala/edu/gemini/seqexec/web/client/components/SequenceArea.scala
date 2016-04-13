package edu.gemini.seqexec.web.client.components

import diode.ModelR
import edu.gemini.seqexec.web.client.components.TabularMenu.TabItem
import edu.gemini.seqexec.web.client.model.{SeqexecCircuit, SequencesOnDisplay}
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconCaretRight, IconPause}

object HeadersSideBar {
  val component = ReactComponentB[Unit]("HeadersSideBar")
    .stateless
    .render(_ =>
      <.div(
        ^.cls := "ui raised secondary segment",
        <.h4("Headers"),
        <.div(
          ^.cls := "ui form",
          <.div(
            ^.cls := "required field",
            <.label("Observer"),
            <.input(
              ^.`type` :="text",
              ^.autoComplete :="off"
            )
          ),
          <.div(
            ^.cls := "required field",
            <.label("SOS"),
            <.input(
              ^.`type` :="text",
              ^.autoComplete :="off"
            )
          ),
          DropdownMenu(DropdownMenu.Props("Image Quality", "Select", List("IQ20", "IQ70", "IQ85", "Any"))),
          DropdownMenu(DropdownMenu.Props("Cloud Cover", "Select", List("CC20", "CC50", "CC70", "CC80", "CC90", "Any"))),
          DropdownMenu(DropdownMenu.Props("Water Vapor", "Select", List("WV20", "WV50", "WV80", "Any"))),
          DropdownMenu(DropdownMenu.Props("Sky Background", "Select", List("SB20", "SB50", "SB80", "Any")))
        )
      )
    )
    .build

  def apply() = component()
}

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

object LogArea {
  val component = ReactComponentB[Unit]("LogArea")
    .stateless
    .render(_ =>
      <.div(
        ^.cls := "ui raised secondary segment",
        <.h4("Log"),
        <.div(
          ^.cls := "ui form",
          <.div(
            ^.cls := "field",
            <.textarea()
          )
        )
      )
    )
    .build

  def apply() = component()
}

object SequenceTabContent {
  case class Props(isActive: Boolean, dataTab: String)

  val component = ReactComponentB[Props]("SequenceTabContent")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui bottom attached active tab segment",
        ^.classSet(
          "active" -> p.isActive
        ),
        dataTab := p.dataTab,
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
                SequenceContainer()
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

  def sequencesTabs(d: SequencesOnDisplay) = d.instrumentSequences.map(a => TabItem(a.instrument, isActive = a == d.focus, a.instrument))
  def tabContents(d: SequencesOnDisplay) = d.instrumentSequences.map(a => SequenceTabContent.Props(isActive = a == d.focus, a.instrument))

  val component = ReactComponentB[Props]("SequenceTabs")
    .stateless
    .render_P( p =>
      <.div(
        ^.cls := "ui bottom attached segment",
        TabularMenu(sequencesTabs(p.sequences)),
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
