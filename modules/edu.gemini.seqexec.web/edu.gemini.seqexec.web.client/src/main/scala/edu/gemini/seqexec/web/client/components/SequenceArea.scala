package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.web.client.components.TabularMenu.TabItem
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

object SequenceArea {
  val component = ReactComponentB[Unit]("QueueTableSection")
    .stateless
    .render( _ =>
      <.div(
        ^.cls := "ui raised segments container",
        TextMenuSegment("Running Sequences"),
        <.div(
          ^.cls := "ui bottom attached segment",
          TabularMenu(List(TabItem("GPI (GS-2016A-Q-0-1)", isActive = true, "GPI"), TabItem("GMOS-S (GS-2016A-Q-5-3)", isActive = false, "GMOS_S"), TabItem("F2 (GS-2016A-Q-4-1)", isActive = false, "F2"))),
          <.div(
            ^.cls := "ui bottom attached active tab segment",
            dataTab := "GPI",
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
                          ),
                          <.tr(
                            ^.cls := "positive",
                            <.td("2"),
                            <.td("Running"),
                            <.td(
                              ^.cls := "collapsing right aligned",
                              IconCaretRight
                            )
                          ),
                          <.tr(
                            <.td("3"),
                            <.td("Pending"),
                            <.td(
                              ^.cls := "collapsing right aligned",
                              IconPause,
                              " ",
                              IconCaretRight
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              <.div(
                ^.cls := "row computer only",
                <.div(
                  ^.cls := "sixteen wide column",
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
              )
            )
          ),
          <.div(
            ^.cls := "ui bottom attached tab segment",
            dataTab := "GMOS_S",
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
              ),
              <.div(
                ^.cls := "row computer only",
                <.div(
                  ^.cls := "sixteen wide column",
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
              )
            )
          )
        )
      )
    ).build

  def apply() = component()
}
