// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.TagOf
import monocle.macros.Lenses
import org.scalajs.dom.html.Div

import scala.concurrent.ExecutionContext.Implicits.global
import seqexec.model.UserDetails
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.model._
import seqexec.web.client.model.SectionVisibilityState._
import seqexec.web.client.actions.CloseLoginBox
import seqexec.web.client.actions.LoggedIn
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.modal.Content
import seqexec.web.client.semanticui.elements.modal.Header
import seqexec.web.client.semanticui.elements.label.FormLabel
import seqexec.web.client.services.SeqexecWebClient
import react.common._
import seqexec.web.client.reusability._

/**
  * UI for the login box
  */
final case class LoginBox(
  visible: SectionVisibilityState
) extends ReactProps {
  @inline def render: VdomElement = LoginBox.component(this)
}

object LoginBox {
  type Props = LoginBox

  @Lenses
  final case class State(username:    String,
                         password:    String,
                         progressMsg: Option[String],
                         errorMsg:    Option[String])

  object State {
    val Empty: State = State("", "", None, None)
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]
  implicit val stateReuse: Reusability[State] = Reusability.derive[State]

  private val formId = "login"

  class Backend(b: BackendScope[Props, State]) {
    def pwdMod(e: ReactEventFromInput): CallbackTo[Unit] = {
      // Capture the value outside setState, react reuses the events
      val v = e.target.value
      b.modState(State.password.set(v))
    }

    def userMod(e: ReactEventFromInput): CallbackTo[Unit] = {
      val v = e.target.value
      b.modState(State.username.set(v))
    }

    def loggedInEvent(u: UserDetails): Callback =
      b.setState(State.Empty) >> SeqexecCircuit.dispatchCB(LoggedIn(u))
    def updateProgressMsg(m: String): Callback =
      b.modState(State.progressMsg.set(m.some) >>> State.errorMsg.set(none))
    def updateErrorMsg(m: String): Callback =
      b.modState(State.errorMsg.set(m.some) >>> State.progressMsg.set(none))
    def closeBox: Callback =
      b.setState(State.Empty) >> SeqexecCircuit.dispatchCB(CloseLoginBox)

    def attemptLogin: Callback = b.state >>= { s =>
      // Change the UI and call login on the remote backend
      updateProgressMsg("Authenticating...") >>
        Callback.future(
          SeqexecWebClient
            .login(s.username, s.password)
            .map(loggedInEvent)
            .recover {
              case _: Exception =>
                updateErrorMsg("Login failed, check username/password")
            }
        )
    }

    private def toolbar(s: State) =
      <.div(
        ^.cls := "ui actions",
        <.div(
          ^.cls := "ui grid",
          <.div(
            ^.cls := "middle aligned row",
            s.progressMsg
              .map(
                m =>
                  <.div(
                    ^.cls := "left floated left aligned six wide column",
                    IconCircleNotched.copyIcon(loading = true),
                    m
                ))
              .whenDefined,
            s.errorMsg
              .map(
                m =>
                  <.div(
                    ^.cls := "left floated left aligned six wide column red",
                    IconAttention,
                    m
                ))
              .whenDefined,
            <.div(
              ^.cls := "right floated right aligned ten wide column",
              Button(onClick    = closeBox)("Cancel"),
              Button(onClick    = attemptLogin,
                     buttonType = Button.SubmitType,
                     form       = Some(formId))("Login")
            )
          )
        )
      )

    def render(s: State): TagOf[Div] =
      <.div(
        ^.cls := "ui modal",
        Header("Login"),
        Content(
          <.form(
            ^.cls := "ui form",
            ^.id := formId,
            ^.method := "post",
            ^.action := "#",
            <.div(
              ^.cls := "required field",
              FormLabel(FormLabel.Props("Username", Some("username"))),
              <.div(
                ^.cls := "ui icon input",
                <.input(
                  ^.`type` := "text",
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
              ^.cls := "required field",
              FormLabel(FormLabel.Props("Password", Some("password"))),
              <.div(
                ^.cls := "ui icon input",
                <.input(
                  ^.`type` := "password",
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
        toolbar(s)
      )
  }

  val component = ScalaComponent
    .builder[Props]("Login")
    .initialState(State.Empty)
    .renderBackend[Backend]
    .componentDidUpdate(ctx =>
      Callback {
        // To properly handle the model we need to do updates with jQuery and
        // the Semantic UI javascript library
        // The calls below use a custom scala.js facade for SemanticUI
        import org.querki.jquery.$
        import web.client.facades.semanticui.SemanticUIModal._

        // Close the modal box if the model changes
        ctx.getDOMNode.toElement.foreach {
          dom =>
            if (ctx.prevProps.visible =!= ctx.currentProps.visible && ctx.currentProps.visible === SectionClosed) {
              $(dom).modal("hide")
            }
            if (ctx.prevProps.visible =!= ctx.currentProps.visible && ctx.currentProps.visible === SectionOpen) {
              // Configure the modal to autofocus and to act properly on closing
              $(dom).modal(
                JsModalOptions
                  .autofocus(true)
                  .onHidden { () =>
                    // Need to call direct access as this is outside the event loop
                    (ctx.setState(State.Empty) *>
                      SeqexecCircuit.dispatchCB(CloseLoginBox)).runNow
                  }
              )
              // Show the modal box
              $(dom).modal("show")
            }
        }
    })
    .configure(Reusability.shouldComponentUpdate)
    .build
}
