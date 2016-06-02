package edu.gemini.seqexec.web.client.components

import diode.react.ModelProxy
import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.web.client.model.{Logout, OpenLoginBox, SeqexecCircuit}
import edu.gemini.seqexec.web.client.semanticui.SemanticUI._
import edu.gemini.seqexec.web.client.semanticui.Size
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.elements.divider.Divider
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconDropdown, IconSignOut}
import edu.gemini.seqexec.web.client.semanticui.elements.menu.Item
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactDOM}
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.ScalaCssReact._

// Former logged in menu, not in use at the moment but it may eventually
// be used if we need to add user settings
object LoggedInMenu {
  case class Props(u: UserDetails)

  val component = ReactComponentB[Props]("SeqexecLoggedInMenu")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui dropdown",
        p.u.displayName,
        IconDropdown,
        <.div(
          ^.cls := "menu",
          Item("Logout"),
          Divider(),
          Item("Settings")
        )
      )
    ).componentDidMount(s =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$

        $(ReactDOM.findDOMNode(s)).dropdown()
      }
    ).build

  def apply(u: UserDetails) = component(Props(u))
}

/**
  * Menu at the top bar
  */
object TopMenu {

  case class Props(u: Option[UserDetails])

  def openLogin = Callback {SeqexecCircuit.dispatch(OpenLoginBox)}
  def logout = Callback {SeqexecCircuit.dispatch(Logout)}

  val loginButton = Button(Button.Props(size = Size.Medium, onClick = openLogin), "Login")
  def logoutButton(text: String) =
    Button(Button.Props(size = Size.Medium, onClick = logout, icon = Some(IconSignOut)), text)

  val component = ReactComponentB[Props]("SeqexecTopMenu")
    .stateless
    .render_P( p =>
      <.div(
        ^.cls := "ui secondary right menu",
        p.u.fold(
          <.div(
            ^.cls := "ui item",
            loginButton
          )
        )(u =>
          <.div(
            ^.cls := "ui secondary right menu",
            <.div(
              ^.cls := "ui header item",
              SeqexecStyles.notInMobile,
              u.displayName
            ),
            <.div(
              ^.cls := "ui header item",
              SeqexecStyles.onlyMobile,
              // Ideally we'd do this with css text-overflow but it is not
              // working properly inside a header item, let's abbreviate in code
              u.displayName.split("\\s").headOption.map(_.substring(0, 10) + "...").getOrElse[String]("")
            ),
            <.div(
              ^.cls := "ui item",
              SeqexecStyles.notInMobile,
              logoutButton("Logout")
            ),
            <.div(
              ^.cls := "ui item",
              SeqexecStyles.onlyMobile,
              logoutButton("")
            )
          )
        )
      )
    )
    .build

  def apply(u: ModelProxy[Option[UserDetails]]) = component(Props(u()))
}
