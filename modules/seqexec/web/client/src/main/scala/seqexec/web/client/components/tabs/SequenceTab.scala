// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.tabs

import cats.implicits._
import gem.Observation
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import react.common.{ Size => _, _ }
import react.semanticui.colors._
import react.semanticui.elements.button.Button
import react.semanticui.elements.icon._
import react.semanticui.elements.label.Label
import react.semanticui.modules.popup.Popup
import react.semanticui.sizes._
import seqexec.model.enum.Instrument
import seqexec.model.Observer
import seqexec.model.RunningStep
import seqexec.model.SequenceState
import seqexec.web.client.actions.LoadSequence
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.icons._
import seqexec.web.client.model.AvailableTab
import seqexec.web.client.model.Pages._
import seqexec.web.client.model.ResourceRunOperation
import seqexec.web.client.model.TabSelected
import seqexec.web.client.reusability._
import seqexec.web.client.semanticui._

final case class SequenceTab(
  router:             RouterCtl[SeqexecPages],
  tab:                AvailableTab,
  loggedIn:           Boolean,
  defaultObserver:    Observer,
  runningInstruments: List[Instrument]
) extends ReactProps[SequenceTab](SequenceTab.component)

object SequenceTab {
  type Props = SequenceTab

  @Lenses
  final case class State(loading: Boolean)

  implicit val propsReuse: Reusability[Props] =
    Reusability.caseClassExcept[Props]("router")
  implicit val stateReuse: Reusability[State] = Reusability.by(_.loading)

  type Backend = RenderScope[Props, State, Unit]

  def load(
    b:    Backend,
    inst: Instrument,
    id:   Observation.Id
  ): (ReactMouseEvent, Button.ButtonProps) => Callback =
    (e: ReactMouseEvent, _: Button.ButtonProps) =>
      e.preventDefaultCB *>
        e.stopPropagationCB *>
        b.setStateL(State.loading)(true) *>
        SeqexecCircuit.dispatchCB(LoadSequence(b.props.defaultObserver, inst, id))

  private def showSequence(p: Props, page: SeqexecPages)(e: ReactEvent): Callback =
    // prevent default to avoid the link jumping
    e.preventDefaultCB *>
      // Request to display the selected sequence
      p.router
        .setUrlAndDispatchCB(page)
        .unless(p.tab.active === TabSelected.Selected)
        .void

  private def linkTo(p: Props, page: SeqexecPages)(mod: TagMod*) = {
    val active     = p.tab.active
    val isPreview  = p.tab.isPreview
    val instrument = p.tab.instrument
    val dataId     = if (isPreview) "preview" else instrument.show
    val hasError   = p.tab.status.isError

    <.a(
      ^.href := p.router.urlFor(page).value,
      ^.onClick ==> showSequence(p, page),
      ^.cls := "item",
      ^.classSet(
        "active" -> (active === TabSelected.Selected)
      ),
      IconAttention.color(Red).when(hasError),
      SeqexecStyles.tab,
      SeqexecStyles.inactiveTabContent.when(active === TabSelected.Background),
      SeqexecStyles.activeTabContent.when(active === TabSelected.Selected),
      SeqexecStyles.errorTab.when(hasError),
      dataTab := dataId,
      mod.toTagMod
    )
  }

  val component = ScalaComponent
    .builder[Props]("SequenceTab")
    .initialState(State(false))
    .render { b =>
      val status     = b.props.tab.status
      val sequenceId = b.props.tab.id
      val instrument = b.props.tab.instrument
      val running    = b.props.runningInstruments.contains(instrument)
      val isPreview  = b.props.tab.isPreview
      val resources = b.props.tab.resourceOperations.filterNot {
        case (r, s) =>
          r.isInstrument || s === ResourceRunOperation.ResourceRunIdle
      }
      val instName = instrument.show
      val dispName = if (isPreview) s"Preview: $instName" else instName
      val isLogged = b.props.loggedIn
      val nextStepToRun =
        StepIdDisplayed(b.props.tab.nextStepToRun.getOrElse(-1))

      val tabTitle = b.props.tab.runningStep match {
        case Some(RunningStep(current, total)) =>
          s"${sequenceId.format} - ${current + 1}/$total"
        case _ =>
          sequenceId.format
      }

      val icon: Icon = status match {
        case SequenceState.Running(_, _) =>
          IconCircleNotched.loading()
        case SequenceState.Completed => IconCheckmark
        case _                       => IconSelectedRadio
      }

      val color = status match {
        case SequenceState.Running(_, _) => Orange
        case SequenceState.Completed     => Green
        case _                           => Grey
      }

      val linkPage: SeqexecPages =
        if (isPreview) {
          PreviewPage(instrument, sequenceId, nextStepToRun)
        } else {
          SequencePage(instrument, sequenceId, nextStepToRun)
        }

      val loadButton: TagMod =
        Popup(
          content = s"Load sequence ${sequenceId.format}",
          trigger = Button(
            size     = Large,
            compact  = true,
            icon     = IconUpload,
            color    = Teal,
            disabled = b.state.loading || running,
            loading  = b.state.loading,
            onClickE = load(b, instrument, sequenceId)
          )
        ).when(isPreview && isLogged)

      val instrumentWithId =
        React.Fragment(
          <.div(SeqexecStyles.activeInstrumentLabel, dispName),
          Label(color = color, clazz = SeqexecStyles.labelPointer)(icon, tabTitle)
        )

      val resourceLabels =
        <.div(
          SeqexecStyles.resourceLabels,
          resources.map {
            case (r, s) =>
              val color = s match {
                case ResourceRunOperation.ResourceRunIdle         => Blue // Unused
                case ResourceRunOperation.ResourceRunCompleted(_) => Green
                case ResourceRunOperation.ResourceRunInFlight(_)  => Yellow
                case ResourceRunOperation.ResourceRunFailed(_)    => Red
              }
              s match {
                case ResourceRunOperation.ResourceRunInFlight(_) =>
                  Label(color = color, size = Small, clazz = SeqexecStyles.activeResourceLabel)(
                    r.show
                  ): VdomNode
                case ResourceRunOperation.ResourceRunCompleted(_) =>
                  Label(color = color, size = Small)(r.show): VdomNode
                case _ => EmptyVdom
              }
          }.toTagMod
        )

      val instrumentAndResources =
        React.Fragment(
          <.div(SeqexecStyles.instrumentAndResourcesLabel,
                <.div(SeqexecStyles.tabLabel, dispName),
                resourceLabels),
          Label(color = color, clazz = SeqexecStyles.labelPointer)(icon, tabTitle)
        )

      val tabContent: VdomNode =
        if (resources.isEmpty) {
          <.div(
            SeqexecStyles.tabLabel,
            instrumentWithId
          )
        } else {
          <.div(
            SeqexecStyles.tabLabel,
            instrumentAndResources
          )
        }

      val previewTabContent: VdomNode =
        <.div(
          SeqexecStyles.previewTabLabel.when(isLogged),
          SeqexecStyles.tabLabel.unless(isLogged),
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
        if (isPreview) previewTabContent
        else tabContent
      )
    }
    .componentWillReceiveProps { f =>
      val preview = f.nextProps.tab.isPreview
      val id      = f.nextProps.tab.id
      val newId   = f.currentProps.tab.id

      val wasLoading = f.currentProps.tab.loading
      val isLoading  = f.nextProps.tab.loading
      // Reset the loading state if the id changes
      f.setStateL(State.loading)(false)
        .when_(preview && (id =!= newId || (wasLoading && !isLoading)))
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
