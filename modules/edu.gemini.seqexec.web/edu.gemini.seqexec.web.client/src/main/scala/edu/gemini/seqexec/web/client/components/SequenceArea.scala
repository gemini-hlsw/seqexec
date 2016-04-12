package edu.gemini.seqexec.web.client.components

import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconAttention, IconDropdown, IconCaretRight, IconPause}

object SequenceArea {
  val component = ReactComponentB[Unit]("QueueTableSection")
    .stateless
    .render( _ =>
      <.div(
        ^.cls := "ui raised segments container",
        <.div(
          ^.cls := "ui top attached text menu segment",
          <.div(
            ^.cls := "ui header item",
            "Running Sequences"
          )
        ),

        <.div(
          ^.cls := "ui bottom attached segment",
          <.div(
            ^.cls := "ui attached tabular menu",
            <.a(
              ^.cls := "active item",
              "GPI (GS-2016A-Q-0-1)"
            ),
            <.a(
              ^.cls := "item",
              "GMOS-S (GS-2016A-Q-5-3)"
            ),
            <.a(
              ^.cls := "item negative",
              IconAttention,
              "F2 (GS-2016A-Q-4-1)"
            )
          ),
          <.div(
            ^.cls := "ui bottom attached active tab segment",
            <.div(
              ^.cls := "ui grid",
              <.div(
                ^.cls := "row",
                <.div(
                  ^.cls := "four wide column tablet computer only",
                  <.div(
                    ^.cls := "ui raised secondary segment",
                    <.h4("Headers"),
                    <.div(
                      ^.cls := "ui form",
                      <.div(
                        ^.cls := "required field error",
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
                      <.div(
                        ^.cls := "field",
                        <.label("Image Quality"),
                        <.div(
                          ^.cls := "ui fluid selection dropdown",
                          <.div(
                            ^.cls := "default text",
                            "Select"
                          ),
                          IconDropdown,
                          <.input(
                            ^.`type` :="hidden",
                            ^.name :="iq"
                          ),
                          <.div(
                            ^.cls := "menu",
                            <.div(
                              ^.cls := "item",
                              "IQ20"
                            ),
                            <.div(
                              ^.cls := "item",
                              "IQ70"
                            ),
                            <.div(
                              ^.cls := "item",
                              "IQ85"
                            ),
                            <.div(
                              ^.cls := "item",
                              "Any"
                            )
                          )
                        )
                      ),
                      <.div(
                        ^.cls := "field",
                        <.label("Cloud Cover"),
                        <.div(
                          ^.cls := "ui fluid selection dropdown",
                          <.div(
                            ^.cls := "default text",
                            "Select"
                          ),
                          IconDropdown,
                          <.input(
                            ^.`type` :="hidden",
                            ^.name :="cc"
                          ),
                          <.div(
                            ^.cls := "menu",
                            <.div(
                              ^.cls := "item",
                              "CC20"
                            ),
                            <.div(
                              ^.cls := "item",
                              "CC50"
                            ),
                            <.div(
                              ^.cls := "item",
                              "CC70"
                            ),
                            <.div(
                              ^.cls := "item",
                              "CC80"
                            ),
                            <.div(
                              ^.cls := "item",
                              "CC90"
                            ),
                            <.div(
                              ^.cls := "item",
                              "Any"
                            )
                          )
                        )
                      ),
                      <.div(
                        ^.cls := "field",
                        <.label("Water Vapor"),
                        <.div(
                          ^.cls := "ui fluid selection dropdown",
                          <.div(
                            ^.cls := "default text",
                            "Select"
                          ),
                          IconDropdown,
                          <.input(
                            ^.`type` :="hidden",
                            ^.name :="wv"
                          ),
                          <.div(
                            ^.cls := "menu",
                            <.div(
                              ^.cls := "item",
                              "WV20"
                            ),
                            <.div(
                              ^.cls := "item",
                              "WV50"
                            ),
                            <.div(
                              ^.cls := "item",
                              "WV80"
                            ),
                            <.div(
                              ^.cls := "item",
                              "Any"
                            )
                          )
                        )
                      ),
                      <.div(
                        ^.cls := "field",
                        <.label("Sky Background"),
                        <.div(
                          ^.cls := "ui fluid selection dropdown",
                          <.div(
                            ^.cls := "default text",
                            "Select"
                          ),
                          IconDropdown,
                          <.input(
                            ^.`type` :="hidden",
                            ^.name :="sb"
                          ),
                          <.div(
                            ^.cls := "menu",
                            <.div(
                              ^.cls := "item",
                              "SB20"
                            ),
                            <.div(
                              ^.cls := "item",
                              "SB50"
                            ),
                            <.div(
                              ^.cls := "item",
                              "SB80"
                            ),
                            <.div(
                              ^.cls := "item",
                              "Any"
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                <.div(
                  ^.cls := "twelve wide computer twelve wide tablet sixteen column",
                  <.div(
                    ^.cls := "ui raised secondary segment",
                    <.div(
                      ^.cls := "row",
                      <.button(
                        ^.cls := "ui button",
                        "Run"
                      ),
                      <.button(
                        ^.cls := "ui button",
                        "Pause"
                      )
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
          )
        )
      )
    ).build

  def apply() = component()
}
