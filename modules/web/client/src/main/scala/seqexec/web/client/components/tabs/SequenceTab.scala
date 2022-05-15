// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.tabs

import cats.syntax.all._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import react.common.{ Size => _, _ }
import react.semanticui.colors._
import react.semanticui.elements.button.Button
import react.semanticui.elements.icon._
import react.semanticui.elements.label.Label
import react.semanticui.modules.popup.Popup
import react.semanticui.sizes._
import seqexec.model.Observation
import seqexec.model.Observer
import seqexec.model.RunningStep
import seqexec.model.SequenceState
import seqexec.model.SystemOverrides
import seqexec.model.enum.Instrument
import seqexec.model.enum.Resource
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
  displayName:        Option[String],
  systemOverrides:    SystemOverrides,
  runningInstruments: List[Instrument]
) extends ReactProps[SequenceTab](SequenceTab.component)

object SequenceTab {
  type Props = SequenceTab

  @Lenses
  final case class State(loading: Boolean, prevTabId: Observation.Id, prevTabLoading: Boolean)

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
        b.setStateL(State.loading)(true).when(b.props.displayName.isDefined) *>
        b.props.displayName
          .map(d => SeqexecCircuit.dispatchCB(LoadSequence(Observer(d), inst, id)))
          .getOrEmpty

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
      ^.href  := p.router.urlFor(page).value,
      ^.onClick ==> showSequence(p, page),
      ^.cls   := "item",
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
    .builder[Props]
    .initialStateFromProps(props => State(false, props.tab.id, props.tab.loading))
    .render { b =>
      val status        = b.props.tab.status
      val sequenceId    = b.props.tab.id
      val instrument    = b.props.tab.instrument
      val running       = b.props.runningInstruments.contains(instrument)
      val isPreview     = b.props.tab.isPreview
      val resources     = b.props.tab.resourceOperations.filterNot { case (r, s) =>
        r.isInstrument || s === ResourceRunOperation.ResourceRunIdle
      }
      val instName      = instrument.show
      val dispName      = if (isPreview) s"Preview: $instName" else instName
      val isLogged      = b.props.loggedIn
      val nextStepToRun =
        StepIdDisplayed(b.props.tab.nextStepToRun.getOrElse(-1))

      val tabTitle = b.props.tab.runningStep match {
        case Some(RunningStep(current, total)) =>
          s"${sequenceId.format} - ${current + 1}/$total"
        case _                                 =>
          sequenceId.format
      }

      val icon: Icon = status match {
        case SequenceState.Running(_, _) =>
          IconCircleNotched.loading()
        case SequenceState.Completed     => IconCheckmark
        case _                           => IconSelectedRadio
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
            size = Large,
            clazz = SeqexecStyles.LoadButton,
            compact = true,
            icon = IconUpload,
            color = Teal,
            disabled = b.state.loading || running,
            loading = b.state.loading,
            onClickE = load(b, instrument, sequenceId)
          )
        ).when(isPreview && isLogged)

      val disabledSubsystems =
        <.div(
          SeqexecStyles.ResourceLabels,
          List(
            ("TCS", b.props.systemOverrides.isTcsEnabled),
            ("GCAL", b.props.systemOverrides.isGcalEnabled),
            ("DHS", b.props.systemOverrides.isDhsEnabled),
            ("INST", b.props.systemOverrides.isInstrumentEnabled)
          ).map { case (l, b) =>
            <.div(SeqexecStyles.DisabledSubsystem, l).unless(b)
          }.toTagMod
        )

      val resourceLabels =
        <.div(
          SeqexecStyles.resourceLabels,
          resources.map { case (r, s) =>
            val show  = r match {
              case Resource.TCS  => b.props.systemOverrides.isTcsEnabled
              case Resource.Gcal => b.props.systemOverrides.isGcalEnabled
              case _: Instrument => b.props.systemOverrides.isInstrumentEnabled
              case _             => true
            }
            val color = s match {
              case ResourceRunOperation.ResourceRunIdle         => Blue // Unused
              case ResourceRunOperation.ResourceRunCompleted(_) => Green
              case ResourceRunOperation.ResourceRunInFlight(_)  => Yellow
              case ResourceRunOperation.ResourceRunFailed(_)    => Red
            }
            (s match {
              case ResourceRunOperation.ResourceRunInFlight(_)  =>
                Label(color = color, size = Small, clazz = SeqexecStyles.activeResourceLabel)(
                  r.show
                ): VdomNode
              case ResourceRunOperation.ResourceRunCompleted(_) =>
                Label(color = color, size = Small)(r.show): VdomNode
              case _                                            => EmptyVdom
            }).when(show)
          }.toTagMod
        )

      val tab =
        <.div(
          SeqexecStyles.TabLabel,
          SeqexecStyles.PreviewTab.when(isLogged),
          SeqexecStyles.LoadedTab.when(!isPreview),
          <.div(
            SeqexecStyles.TabTitleRow,
            dispName,
            disabledSubsystems.when(!isPreview),
            resourceLabels.when(!isPreview)
          ),
          Label(color = color, clazz = SeqexecStyles.labelPointer)(icon, tabTitle).when(!isPreview),
          loadButton.when(isPreview)
        )

      linkTo(b.props, linkPage)(tab)
    }
    .getDerivedStateFromProps { (props, state) =>
      val preview = props.tab.isPreview
      val id      = state.prevTabId
      val newId   = props.tab.id

      val wasLoading = state.prevTabLoading
      val isLoading  = props.tab.loading
      // Reset the loading state if the id changes
      Function.chain(
        State.loading
          .set(false)
          .some
          .filter(_ => preview && (id =!= newId || wasLoading && !isLoading))
          .toList :::
          List(
            State.prevTabId.set(newId),
            State.prevTabLoading.set(isLoading)
          )
      )(state)
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
