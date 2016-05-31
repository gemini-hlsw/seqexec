package edu.gemini.seqexec.web.client.components

import diode.react.ModelProxy
import edu.gemini.seqexec.model.UserDetails
import japgolly.scalajs.react.{BackendScope, Callback, ReactComponentB, ReactDOM}
import japgolly.scalajs.react.vdom.prefix_<^._
import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.elements.input.Input
import edu.gemini.seqexec.web.client.semanticui.elements.input.Input.ChangeCallback
import edu.gemini.seqexec.web.client.services.SeqexecWebClient

import scala.concurrent.ExecutionContext.Implicits.global

object LoginBox {

  case class Props(open: ModelProxy[SectionVisibilityState])

  case class State(username: String, password: String, progressMsg: Option[String], errorMsg: Option[String])

  def pwdInput(callback: ChangeCallback) = Input(Input.Props("password", "password", Input.PasswordInput, "Password", onChange = callback))

  class Backend($: BackendScope[Props, State]) {
    def pwdMod = (s: String) => $.modState(_.copy(password = s))
    def userMod = (s: String) => $.modState(_.copy(username = s))

    def loggedInEvent(u: UserDetails):Callback = Callback {SeqexecCircuit.dispatch(LoggedIn(u))} >> updateProgressMsg("")
    def updateProgressMsg(m: String):Callback = $.modState(_.copy(progressMsg = Some(m), errorMsg = None))
    def updateErrorMsg(m: String):Callback = $.modState(_.copy(errorMsg = Some(m), progressMsg = None))

    def attemptLogin = $.state >>= { s =>
      updateProgressMsg("Contacting server...") >>
      Callback.future(
        SeqexecWebClient.login(s.username, s.password)
          .map(loggedInEvent)
          .recover {
            case t: Exception => updateErrorMsg("Login failed, check username/password")
          }
      )
    }

    val usernameInput = Input(Input.Props("username", "username", Input.TextInput, "Username", onChange = userMod))

    val passwordInput = Input(Input.Props("password", "password", Input.PasswordInput, "Password", onChange = pwdMod))

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
            <.div(
              ^.cls :="required field",
              <.label(
                ^.htmlFor := "username",
                "Username: "
              ),
              <.div(
                ^.cls :="ui icon input",
                usernameInput,
                IconUser
              )
            ),
            <.div(
              ^.cls :="required field",
              <.label(
                ^.htmlFor := "password",
                "Password: "
              ),
              <.div(
                ^.cls := "ui icon input",
                passwordInput,
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
                <.div(
                  ^.cls := "ui cancel button",
                  "Cancel"
                ),
                Button(Button.Props(onClick = attemptLogin), "Login")
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
              .onDeny { () =>
                // Called when cancel is pressed
                SeqexecCircuit.dispatch(CloseLoginBox)
              }
          )
          $(ReactDOM.findDOMNode(s.$)).modal("show")
        }
      }
    ).build

  def apply(s: ModelProxy[SectionVisibilityState]) = component(Props(s))
}
