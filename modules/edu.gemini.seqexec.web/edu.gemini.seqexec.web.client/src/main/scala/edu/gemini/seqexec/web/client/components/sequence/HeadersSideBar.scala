package edu.gemini.seqexec.web.client.components.sequence

import edu.gemini.seqexec.web.client.components.DropdownMenu
import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._

/**
  * Display to show headers per sequence
  */
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
            <.label("Operator"),
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
