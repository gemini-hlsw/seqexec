package edu.gemini.seqexec.web.client.components.sequence

import diode.react.ModelProxy

import edu.gemini.seqexec.web.client.components.DropdownMenu
import edu.gemini.seqexec.web.client.semanticui.elements.label.Label
import edu.gemini.seqexec.web.client.semanticui.elements.input.Input
import edu.gemini.seqexec.web.client.model._

import japgolly.scalajs.react.{ReactComponentB, Callback}
import japgolly.scalajs.react.vdom.prefix_<^._

/**
  * Display to show headers per sequence
  */
object HeadersSideBar {
  case class Props(operator: ModelProxy[Option[String]], status: ModelProxy[ClientStatus])

  val component = ReactComponentB[Props]("HeadersSideBar")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui raised secondary segment",
        <.h4("Headers"),
        <.div(
          ^.cls := "ui form",
          <.div(
            ^.cls := "required field",
            Label(Label.Props("Operator", "operator")),
            Input(Input.Props("operator", "operator",
              p.operator().getOrElse(""),
              placeholder = "Operator...",
              disabled = !p.status().isLogged,
              onBlur = name => p.operator.dispatchCB(UpdateOperator(name))))
          ),
          DropdownMenu(DropdownMenu.Props("Image Quality", "Select", List("IQ20", "IQ70", "IQ85", "Any"))),
          DropdownMenu(DropdownMenu.Props("Cloud Cover", "Select", List("CC20", "CC50", "CC70", "CC80", "CC90", "Any"))),
          DropdownMenu(DropdownMenu.Props("Water Vapor", "Select", List("WV20", "WV50", "WV80", "Any"))),
          DropdownMenu(DropdownMenu.Props("Sky Background", "Select", List("SB20", "SB50", "SB80", "Any")))
        )
      )
    )
    .build

  def apply(operator: ModelProxy[Option[String]], status: ModelProxy[ClientStatus]) = component(Props(operator, status))
}
