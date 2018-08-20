// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence

import cats.implicits._
import gem.Observation
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import seqexec.model.{Observer, UserDetails, SequenceState}
import seqexec.model.enum.Instrument
import seqexec.web.client.actions.LoadSequence
import seqexec.web.client.model.Pages._
import seqexec.web.client.model.{ AvailableTab, RunningStep }
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.semanticui._
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.semanticui.elements.label.Label
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.popup.Popup
import seqexec.web.client.components.SeqexecStyles
import web.client.style._

object InstrumentTab {
  final case class Props(router: RouterCtl[SeqexecPages], tab: AvailableTab, loggedIn: Boolean, user: Option[UserDetails])
  final case class State(loading: Boolean)

  type Backend = RenderScope[Props, State, Unit]

  def load(b: Backend, inst: Instrument, id: Observation.Id): Callback =
    b.setState(State(loading = true)) *>
    b.props.user.map(u => SeqexecCircuit.dispatchCB(LoadSequence(Observer(u.displayName), inst, id))).getOrEmpty

  private def showSequence(p: Props, page: SeqexecPages)(e: ReactEvent): Callback = {
    // prevent default to avoid the link jumping
    e.preventDefault
    // Request to display the selected sequence
    p.router.setUrlAndDispatchCB(page)
  }

  private def linkTo(p: Props, page: SeqexecPages)(mod: TagMod*) = {
    val active = p.tab.active
    val isPreview = p.tab.isPreview
    val instrument = p.tab.instrument
    val dataId = if (isPreview) "preview" else instrument.foldMap(_.show)
    val hasError = p.tab.status.exists(_.isError)

    <.a(
      ^.href := p.router.urlFor(page).value,
      ^.onClick ==> showSequence(p, page),
      ^.cls := "item",
      ^.classSet(
        "active" -> active
      ),
      IconAttention.copyIcon(color = Some("red")).when(hasError),
      SeqexecStyles.instrumentTab,
      SeqexecStyles.inactiveInstrumentContent.unless(active),
      SeqexecStyles.activeInstrumentContent.when(active),
      dataTab := dataId,
      SeqexecStyles.errorTab.when(hasError),
      mod.toTagMod
    )
  }

  private val component = ScalaComponent.builder[Props]("SequenceTab")
    .initialState(State(false))
    .render { b =>
      val status = b.props.tab.status
      val sequenceId = b.props.tab.id
      val instrument = b.props.tab.instrument
      val isPreview = b.props.tab.isPreview
      val instName = instrument.foldMap(_.show)
      val dispName = if (isPreview) s"Preview: $instName" else instName
      val isLogged = b.props.loggedIn

      val tabTitle = b.props.tab.runningStep match {
        case Some(RunningStep(current, total)) => s"${sequenceId.map(_.format).getOrElse("")} - ${current + 1}/$total"
        case _                                 => sequenceId.map(_.format).getOrElse("Empty")
      }

      val icon = status.flatMap {
        case SequenceState.Running(_, _) => IconCircleNotched.copyIcon(loading = true).some
        case SequenceState.Completed     => IconCheckmark.some
        case _                           => IconSelectedRadio.some
      }

      val color = status.flatMap {
        case SequenceState.Running(_, _) => "orange".some
        case SequenceState.Completed     => "green".some
        case _                           => "grey".some
      }

      val linkPage: SeqexecPages =
        (sequenceId, instrument)
          .mapN((id, inst) => if (isPreview) PreviewPage(inst, id, 0) else SequencePage(inst, id, 0))
          .getOrElse(EmptyPreviewPage)

      val loadButton: Option[VdomNode] =
        (sequenceId, instrument)
          .mapN((id, inst) =>
            Popup(Popup.Props("button", s"Load sequence ${id.format}"),
              Button(
                Button.Props(
                  size = Size.Large,
                  compact = true,
                  icon = Some(IconSignIn),
                  color = "teal".some,
                  disabled = b.state.loading,
                  loading = b.state.loading,
                  onClick = load(b, inst, id))
              )
            ): VdomNode)
          .filter(_ => isPreview && isLogged)

      val instrumentWithId =
        ReactFragment(
          <.div(SeqexecStyles.activeInstrumentLabel, dispName),
          Label(Label.Props(tabTitle, color = color, icon = icon, extraStyles = List(SeqexecStyles.labelPointer)))
        )

      val tabContent: VdomNode =
        <.div(
          SeqexecStyles.instrumentTabLabel,
          instrumentWithId
        )

      val previewTabContent: VdomNode =
        <.div(
          SeqexecStyles.previewTabLabel.when(isLogged && sequenceId.isDefined),
          SeqexecStyles.instrumentTabLabel.unless(isLogged),
          SeqexecStyles.instrumentTabLabel.when(sequenceId.isEmpty),
          <.div(
            SeqexecStyles.previewTabId,
            instrumentWithId
          ),
          <.div(
            SeqexecStyles.previewTabLoadButton,
            loadButton
          )
        )

      linkTo(b.props, linkPage)(if (isPreview) previewTabContent else tabContent)
    }.build

  def apply(p: Props): Unmounted[Props, State, Unit] = component(p)
}
/**
  * Menu with tabs
  */
object InstrumentsTabs {
  final case class Props(router: RouterCtl[SeqexecPages], loggedIn: Boolean)

  private val component = ScalaComponent.builder[Props]("InstrumentsMenu")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui attached tabular menu",
        SeqexecCircuit.connect(SeqexecCircuit.tabsReader)(x => ReactFragment(x().tabs.toList.map(t => InstrumentTab(InstrumentTab.Props(p.router, t, p.loggedIn, x().user)): VdomNode): _*))
      )
    ).build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
