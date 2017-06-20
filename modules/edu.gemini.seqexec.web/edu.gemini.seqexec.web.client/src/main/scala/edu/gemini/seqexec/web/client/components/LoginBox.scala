package edu.gemini.seqexec.web.client.components

import diode.react.ModelProxy
import edu.gemini.seqexec.model.UserDetails
import japgolly.scalajs.react.{BackendScope, Callback, CallbackTo, ReactEventFromInput, ScalaComponent}
import japgolly.scalajs.react.vdom.html_<^._
import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.elements.label.FormLabel
import edu.gemini.seqexec.web.client.services.SeqexecWebClient
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.TagOf
import org.scalajs.dom.html.Div

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
    def pwdMod(e: ReactEventFromInput): CallbackTo[Unit] = {
      // Capture the value outside setState, react reuses the events
      val v = e.target.value
      $.modState(_.copy(password = v))
    }

    def userMod(e: ReactEventFromInput): CallbackTo[Unit] = {
      val v = e.target.value
      $.modState(_.copy(username = v))
    }

    def loggedInEvent(u: UserDetails):Callback = Callback { SeqexecCircuit.dispatch(LoggedIn(u)) } >> updateProgressMsg("")
    def updateProgressMsg(m: String):Callback = $.modState(_.copy(progressMsg = Some(m), errorMsg = None))
    def updateErrorMsg(m: String):Callback = $.modState(_.copy(errorMsg = Some(m), progressMsg = None))
    def closeBox:Callback = $.modState(_ => empty) >> Callback {SeqexecCircuit.dispatch(CloseLoginBox)}

    def attemptLogin:Callback = $.state >>= { s =>
      // Change the UI and call login on the remote backend
      updateProgressMsg("Authenticating...") >>
      Callback.future(
        SeqexecWebClient.login(s.username, s.password)
          .map(loggedInEvent)
          .recover {
            case t: Exception => updateErrorMsg("Login failed, check username/password")
          }
      )
    }

    def render(p: Props, s: State): TagOf[Div] =
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
              FormLabel(FormLabel.Props("Username", Some("username"))),
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
              FormLabel(FormLabel.Props("Password", Some("password"))),
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
              ).whenDefined,
              s.errorMsg.map( m =>
                <.div(
                  ^.cls := "left floated left aligned six wide column red",
                  IconAttention,
                  m
                )
              ).whenDefined,
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

  private val component = ScalaComponent.builder[Props]("Login")
    .initialState(State("", "", None, None))
    .renderBackend[Backend]
    .componentDidUpdate(ctx =>
      Callback {
        // To properly handle the model we need to do updates with jQuery and
        // the Semantic UI javascript library
        // The calls below use a custom scala.js facade for SemantiUI
        import org.querki.jquery.$

        // Close the modal box if the model changes
        if (ctx.currentProps.open() == SectionClosed) {
          $(ctx.getDOMNode).modal("hide")
        }
        if (ctx.currentProps.open() == SectionOpen) {
          // Configure the modal to autofoucs and to act properly on closing
          $(ctx.getDOMNode).modal(
            JsModalOptions
              .autofocus(true)
              .onHidden { () =>
                // Need to call direct access as this is outside the event loop
                ctx.setState(empty)
                SeqexecCircuit.dispatch(CloseLoginBox)
              }
          )
          // Show the modal box
          $(ctx.getDOMNode).modal("show")
        }
      }
    ).build

  def apply(s: ModelProxy[SectionVisibilityState]): Unmounted[Props, State, Backend] = component(Props(s))
}
