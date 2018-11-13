// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.tabs

import cats.implicits._
import gem.Observation
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react._
import seqexec.model.{ Observer, SequenceState }
import seqexec.model.enum.Instrument
import seqexec.web.client.actions.LoadSequence
import seqexec.web.client.model.Pages._
import seqexec.web.client.model.{ AvailableTab, RunningStep, TabSelected }
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.semanticui._
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.semanticui.elements.label.Label
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.popup.Popup
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.reusability._
import web.client.style._

object SequenceTab {
  final case class Props(router:             RouterCtl[SeqexecPages],
                         tab:                AvailableTab,
                         loggedIn:           Boolean,
                         defaultObserver:    Observer,
                         runningInstruments: List[Instrument])
  final case class State(loading:            Boolean)

  implicit val propsReuse: Reusability[Props] =
    Reusability.by(x =>
      (x.tab, x.loggedIn, x.defaultObserver, x.runningInstruments))
  implicit val stateReuse: Reusability[State] = Reusability.by(_.loading)

  type Backend = RenderScope[Props, State, Unit]

  def load(b: Backend, inst: Instrument, id: Observation.Id)(e: ReactEvent): Callback =
    e.preventDefaultCB *>
      e.stopPropagationCB *>
      b.setState(State(loading = true)) *>
      SeqexecCircuit.dispatchCB(LoadSequence(b.props.defaultObserver, inst, id))

  private def showSequence(p: Props, page: SeqexecPages)(e: ReactEvent): Callback =
    // prevent default to avoid the link jumping
    e.preventDefaultCB *>
      // Request to display the selected sequence
      p.router
        .setUrlAndDispatchCB(page)
        .unless(p.tab.active === TabSelected.Selected) *>
      Callback.empty

  private def linkTo(p: Props, page: SeqexecPages)(mod: TagMod*) = {
    val active     = p.tab.active
    val isPreview  = p.tab.isPreview
    val instrument = p.tab.instrument
    val dataId     = if (isPreview) "preview" else instrument.foldMap(_.show)
    val hasError   = p.tab.status.exists(_.isError)

    <.a(
      ^.href := p.router.urlFor(page).value,
      ^.onClick ==> showSequence(p, page),
      ^.cls := "item",
      ^.classSet(
        "active" -> (active === TabSelected.Selected)
      ),
      IconAttention.copyIcon(color = Some("red")).when(hasError),
      SeqexecStyles.tab,
      SeqexecStyles.inactiveTabContent.when(active === TabSelected.Background),
      SeqexecStyles.activeTabContent.when(active === TabSelected.Selected),
      SeqexecStyles.errorTab.when(hasError),
      dataTab := dataId,
      mod.toTagMod
    )
  }

  private val component = ScalaComponent
    .builder[Props]("SequenceTab")
    .initialState(State(false))
    .render { b =>
      val status     = b.props.tab.status
      val sequenceId = b.props.tab.id
      val instrument = b.props.tab.instrument
      val running    = instrument.exists(b.props.runningInstruments.contains)
      val isPreview  = b.props.tab.isPreview
      val instName   = instrument.foldMap(_.show)
      val dispName   = if (isPreview) s"Preview: $instName" else instName
      val isLogged   = b.props.loggedIn
      val nextStepToRun =
        StepIdDisplayed(b.props.tab.nextStepToRun.getOrElse(-1))

      val tabTitle = b.props.tab.runningStep match {
        case Some(RunningStep(current, total)) =>
          s"${sequenceId.map(_.format).getOrElse("")} - ${current + 1}/$total"
        case _                                 =>
          sequenceId.map(_.format).getOrElse("Empty")
      }

      val icon = status.flatMap {
        case SequenceState.Running(_, _) =>
          IconCircleNotched.copyIcon(loading = true).some
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
          .mapN((id, inst) =>
            if (isPreview) {
              PreviewPage(inst, id, nextStepToRun)
            } else {
              SequencePage(inst, id, nextStepToRun)
          })
          .getOrElse(CalibrationQueuePage)

      val loadButton: Option[VdomNode] =
        (sequenceId, instrument)
          .mapN((id, inst) =>
            Popup(
              Popup.Props("button", s"Load sequence ${id.format}"),
              Button(
                Button.Props(
                  size     = Size.Large,
                  compact  = true,
                  icon     = Some(IconUpload),
                  color    = "teal".some,
                  disabled = b.state.loading || running,
                  loading  = b.state.loading,
                  onClickE = load(b, inst, id) _
                )
              )
            ): VdomNode)
          .filter(_ => isPreview && isLogged)

      val instrumentWithId =
        React.Fragment(
          <.div(SeqexecStyles.activeInstrumentLabel, dispName),
          Label(
            Label.Props(tabTitle,
                        color       = color,
                        icon        = icon,
                        extraStyles = List(SeqexecStyles.labelPointer)))
        )

      val tabContent: VdomNode =
        <.div(
          SeqexecStyles.tabLabel,
          instrumentWithId
        )

      val previewTabContent: VdomNode =
        <.div(
          SeqexecStyles.previewTabLabel.when(isLogged && sequenceId.isDefined),
          SeqexecStyles.tabLabel.unless(isLogged),
          SeqexecStyles.tabLabel.when(sequenceId.isEmpty),
          <.div(
            SeqexecStyles.previewTabId,
            instrumentWithId
          ),
          <.div(
            SeqexecStyles.previewTabLoadButton,
            loadButton
          )
        )

      linkTo(b.props, linkPage)(
        if (isPreview) previewTabContent else tabContent)
    }
    .componentWillReceiveProps { f =>
      val preview = f.nextProps.tab.isPreview
      val id      = f.nextProps.tab.id
      val newId   = f.currentProps.tab.id

      val wasLoading = f.currentProps.tab.loading
      val isLoading  = f.nextProps.tab.loading
      // Reset the loading state if the id changes
      Callback.when(preview && (id =!= newId || (wasLoading && !isLoading)))(
        f.setState(State(false)))
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, State, Unit] = component(p)
}
