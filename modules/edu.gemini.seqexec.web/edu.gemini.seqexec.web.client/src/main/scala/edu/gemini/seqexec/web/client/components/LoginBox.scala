package edu.gemini.seqexec.web.client.components

import diode.react.ModelProxy
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactDOM}
import japgolly.scalajs.react.vdom.prefix_<^._
import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconUser, IconLock}
import edu.gemini.seqexec.web.client.model.{SectionOpen, SectionVisibilityState}

/**
  * Created by cquiroz on 5/27/16.
  */
object LoginBox {

  case class Props(open: ModelProxy[SectionVisibilityState])

  val component = ReactComponentB[Props]("Login")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui modal",
        <.div(
          ^.cls := "header",
          "Login"
        ),
        <.div(
          ^.cls := "ui content",
          <.form(
            ^.cls :="ui form",
            <.div(
              ^.cls :="field",
              <.label(
                ^.htmlFor := "username",
                "Username: "
              ),
              <.div(
                ^.cls :="ui icon input",
                <.input(
                  ^.`type` := "text",
                  ^.placeholder := "Username",
                  ^.name := "username",
                  ^.id := "username"
                ),
                IconUser
              )
            ),
            <.div(
              ^.cls :="field",
              <.label(
                ^.htmlFor := "password",
                "Password: "
              ),
              <.div(
                ^.cls :="ui icon input",
                <.input(
                  ^.`type` :="password",
                  ^.placeholder := "Password",
                  ^.name := "password",
                  ^.id := "password"
                ),
                IconLock
              )
            )
          )
        ),
        <.div(
          ^.cls := "actions",
            <.div(
              ^.cls := "ui button",
              "Cancel"
            ),
            <.div(
              ^.cls := "ui button",
              "Login"
            )
        )
      )
    ).componentDidUpdate(s =>
      Callback {
        import org.querki.jquery.$

        // Open the modal box
        if (s.currentProps.open() == SectionOpen) {
          $(ReactDOM.findDOMNode(s.$)).modal("show")
        }
      }
    ).build

  def apply(s: ModelProxy[SectionVisibilityState]) = component(Props(s))
}
