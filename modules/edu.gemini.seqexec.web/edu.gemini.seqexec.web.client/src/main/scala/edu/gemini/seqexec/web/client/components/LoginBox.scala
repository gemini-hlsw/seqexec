package edu.gemini.seqexec.web.client.components

import diode.react.ModelProxy
import edu.gemini.seqexec.model.UserDetails
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactDOM, ReactEventI}
import japgolly.scalajs.react.vdom.prefix_<^._
import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.elements.label.Label
import edu.gemini.seqexec.web.client.services.SeqexecWebClient

import scala.concurrent.ExecutionContext.Implicits.global

/**
  * UI for the login box
  */
object LoginBox {

  case class Props(open: ModelProxy[SectionVisibilityState])

  case class State(username: String, password: String, progressMsg: Option[String], errorMsg: Option[String])

  val empty = State("", "", None, None)

  val formId = "login"

  class Backend($: BackendScope[Props, State]) {
    def pwdMod(e: ReactEventI) = {
      // Capture the value outside setState, react reuses the events
      val v = e.target.value
      $.modState(_.copy(password = v))
    }

    def userMod(e: ReactEventI) = {
      // Capture the value outside setState, react reuses the events
      val v = e.target.value
      $.modState(_.copy(username = v))
    }

    def loggedInEvent(u: UserDetails):Callback = Callback {SeqexecCircuit.dispatch(LoggedIn(u))} >> updateProgressMsg("")
    def updateProgressMsg(m: String):Callback = $.modState(_.copy(progressMsg = Some(m), errorMsg = None))
    def updateErrorMsg(m: String):Callback = $.modState(_.copy(errorMsg = Some(m), progressMsg = None))
    def closeBox = $.modState(_ => empty) >> Callback {SeqexecCircuit.dispatch(CloseLoginBox)}

    def attemptLogin = $.state >>= { s =>
      updateProgressMsg("Authenticating...") >>
      Callback.future(
        SeqexecWebClient.login(s.username, s.password)
          .map(loggedInEvent)
          .recover {
            case t: Exception => updateErrorMsg("Login failed, check username/password")
          }
      )
    }

    def render(p: Props, s: State) =
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
            ^.id := formId,
            ^.method := "post",
            ^.action := "#",
            <.div(
              ^.cls :="required field",
              Label(Label.Props("Username", "username")),
              <.div(
                ^.cls :="ui icon input",
                <.input(
                  ^.`type` :="text",
                  ^.placeholder := "Username",
                  ^.name := "username",
                  ^.id := "username",
                  ^.value := s.username,
                  ^.onChange ==> userMod
                ),
                IconUser
              )
            ),
            <.div(
              ^.cls :="required field",
              Label(Label.Props("Password", "password")),
              <.div(
                ^.cls := "ui icon input",
                <.input(
                  ^.`type` :="password",
                  ^.placeholder := "Password",
                  ^.name := "password",
                  ^.id := "password",
                  ^.value := s.password,
                  ^.onChange ==> pwdMod
                ),
                IconLock
              )
            )
          )
        ),
        <.div(
          ^.cls := "ui actions",
          <.div(
            ^.cls := "ui grid",
            <.div(
              ^.cls := "middle aligned row",
              s.progressMsg.map( m =>
                <.div(
                  ^.cls := "left floated left aligned six wide column",
                  IconCircleNotched.copyIcon(loading = true),
                  m
                )
              ),
              s.errorMsg.map( m =>
                <.div(
                  ^.cls := "left floated left aligned six wide column red",
                  IconAttention,
                  m
                )
              ),
              <.div(
                ^.cls := "right floated right aligned ten wide column",
                Button(Button.Props(onClick = closeBox), "Cancel"),
                Button(Button.Props(onClick = attemptLogin, buttonType = Button.SubmitType, form = Some(formId)), "Login")
              )
            )
          )
        )
      )
  }

  val component = ReactComponentB[Props]("Login")
    .initialState(State("", "", None, None))
    .renderBackend[Backend]
    .componentDidUpdate(s =>
      Callback {
        import org.querki.jquery.$

        // Open the modal box
        if (s.currentProps.open() == SectionClosed) {
          $(ReactDOM.findDOMNode(s.$)).modal("hide")
        }
        if (s.currentProps.open() == SectionOpen) {
          $(ReactDOM.findDOMNode(s.$)).modal(
            JsModalOptions
              .autofocus(true)
              .onHidden { () =>
                // Need to call direct access as this is outside the event loop
                s.$.accessDirect.setState(empty)
                SeqexecCircuit.dispatch(CloseLoginBox)
              }
          )
          $(ReactDOM.findDOMNode(s.$)).modal("show")
        }
      }
    ).build

  def apply(s: ModelProxy[SectionVisibilityState]) = component(Props(s))
}
