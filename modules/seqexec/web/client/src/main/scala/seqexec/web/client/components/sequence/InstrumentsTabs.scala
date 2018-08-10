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
import seqexec.model.SequenceState
import seqexec.model.enum.Instrument
import seqexec.web.client.actions.{LoadSequence, NavigateTo, SelectIdToDisplay, SelectInstrumentToDisplay}
import seqexec.web.client.ModelOps._
import seqexec.web.client.model.Pages.{InstrumentPage, SequencePage, SeqexecPages}
import seqexec.web.client.model.AvailableTab
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.semanticui._
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.semanticui.elements.label.Label
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.popup.Popup
import seqexec.web.client.components.SeqexecStyles
import web.client.style._

object InstrumentTab {
  final case class Props(router: RouterCtl[SeqexecPages], tab: AvailableTab, loggedIn: Boolean)
  final case class State(loading: Boolean)

  type Backend = RenderScope[Props, State, Unit]

  def load(b: Backend, inst: Instrument, id: Observation.Id): Callback =
    b.setState(State(loading = true)) *> SeqexecCircuit.dispatchCB(LoadSequence(inst, id))

  private val component = ScalaComponent.builder[Props]("SequenceTab")
    .initialState(State(false))
    .render { b =>
      val active = b.props.tab.active
      val status = b.props.tab.status
      val hasError = status.exists(_.isError)
      val sequenceId = b.props.tab.id
      val instrument = b.props.tab.instrument
      val isPreview = b.props.tab.isPreview
      val instName = instrument.foldMap(_.show)
      val dispName = if (isPreview) s"Preview: $instName" else instName

      val tabTitle = b.props.tab.runningStep match {
        case Some(RunningStep(current, total)) => s"${sequenceId.map(_.format).getOrElse("")} - ${current + 1}/$total"
        case _                                 => sequenceId.map(_.format).getOrElse("")
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

      val linkPage: Option[SeqexecPages] =
        (sequenceId, instrument)
          .mapN((id, inst) => SequencePage(inst, id, 0))
          .filter(_ => !isPreview)


      val loadButton: Option[VdomNode] =
        (sequenceId, instrument)
          .mapN((id, inst) =>
            Popup(Popup.Props("button", s"Load sequence ${id.format}"),
              Button(
                Button.Props(
                  size = Size.Large,
                  compact = true,
                  icon = Some(IconSignIn),
                  disabled = b.state.loading,
                  loading = b.state.loading,
                  onClick = load(b, inst, id))
              )
            ): VdomNode)
          .filter(_ => isPreview && b.props.loggedIn)

      val labelNoId =
        <.div(SeqexecStyles.instrumentTabLabel, if (isPreview) "Preview" else dispName)

      val instrumentWithId =
        <.div(
          SeqexecStyles.instrumentTabLabel,
          <.div(SeqexecStyles.activeInstrumentLabel, dispName),
          Label(Label.Props(tabTitle, color = color, icon = icon, extraStyles = if (isPreview) Nil else List(SeqexecStyles.labelPointer)))
        )

      val dataId = if (isPreview) "preview" else instrument.show

      val tabContent: VdomNode =
        <.div(
          IconAttention.copyIcon(color = Some("red")).when(hasError),
          sequenceId.fold(labelNoId)(_ => instrumentWithId),
          ^.cls := "item",
          ^.classSet(
            "active" -> active,
            "error"  -> hasError
          ),
          SeqexecStyles.instrumentTab,
          SeqexecStyles.inactiveInstrumentContent.unless(active),
          SeqexecStyles.activeInstrumentContent.when(active),
          SeqexecStyles.errorTab.when(hasError),
          dataTab := dataId,
          loadButton
        )

      linkPage.map(l => b.props.router.link(l)(tabContent): VdomNode).getOrElse(tabContent)
    }.componentDidMount(ctx =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$
        import web.client.facades.semanticui.SemanticUITab._

        ctx.getDOMNode.toElement.foreach { dom =>
          $(dom).tab(
            JsTabOptions
              .onVisible { (x: String) =>
                val sequenceId = ctx.props.tab.id
                val instrument = ctx.props.tab.instrument
                val updateModelCB = (sequenceId, instrument) match {
                  case (Some(id), Some(i)) =>
                    SeqexecCircuit.dispatchCB(NavigateTo(SequencePage(i, id, 0))) >> SeqexecCircuit.dispatchCB(SelectIdToDisplay(id))
                  case (_, Some(i))        =>
                    SeqexecCircuit.dispatchCB(NavigateTo(InstrumentPage(i))) >> SeqexecCircuit.dispatchCB(SelectInstrumentToDisplay(i))
                  case _                   =>
                    Callback.empty
                }
                // runNow as we are outside react loop
                updateModelCB.runNow()
              }
          )
        }
      }
    ).build

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
        SeqexecCircuit.connect(SeqexecCircuit.availableTabs)(x => ReactFragment(x().toList.map(t => InstrumentTab(InstrumentTab.Props(p.router, t, p.loggedIn)): VdomNode): _*))
      )
    ).build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
