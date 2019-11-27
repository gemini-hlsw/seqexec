// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.tabs

import cats.implicits._
import gem.Observation
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react._
import react.common._
import react.common.implicits._
import seqexec.model.Observer
import seqexec.model.SequenceState
import seqexec.model.enum.Instrument
import seqexec.model.RunningStep
import seqexec.web.client.actions.LoadSequence
import seqexec.web.client.model.Pages._
import seqexec.web.client.model.AvailableTab
import seqexec.web.client.model.TabSelected
import seqexec.web.client.model.ResourceRunOperation
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.semanticui._
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.semanticui.elements.label.Label
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.popup.Popup
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.reusability._

final case class SequenceTab(
  router:             RouterCtl[SeqexecPages],
  tab:                AvailableTab,
  loggedIn:           Boolean,
  defaultObserver:    Observer,
  runningInstruments: List[Instrument]
) extends ReactProps {
  @inline def render: VdomElement = SequenceTab.component(this)
}

object SequenceTab {
  type Props = SequenceTab

  final case class State(loading:            Boolean)

  implicit val propsReuse: Reusability[Props] =
    Reusability.caseClassExcept[Props]('router)
  implicit val stateReuse: Reusability[State] = Reusability.by(_.loading)

  type Backend = RenderScope[Props, State, Unit]

  def load(b: Backend, inst: Instrument, id: Observation.Id)(
    e:        ReactEvent): Callback =
    e.preventDefaultCB *>
      e.stopPropagationCB *>
      b.setState(State(loading = true)) *>
      SeqexecCircuit.dispatchCB(LoadSequence(b.props.defaultObserver, inst, id))

  private def showSequence(p: Props, page: SeqexecPages)(
    e:                        ReactEvent): Callback =
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
    val dataId     = if (isPreview) "preview" else instrument.show
    val hasError   = p.tab.status.isError

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

      val icon = status match {
        case SequenceState.Running(_, _) =>
          IconCircleNotched.copyIcon(loading = true)
        case SequenceState.Completed => IconCheckmark
        case _                       => IconSelectedRadio
      }

      val color = status match {
        case SequenceState.Running(_, _) => "orange"
        case SequenceState.Completed     => "green"
        case _                           => "grey"
      }

      val linkPage: SeqexecPages =
        if (isPreview) {
          PreviewPage(instrument, sequenceId, nextStepToRun)
        } else {
          SequencePage(instrument, sequenceId, nextStepToRun)
        }

      val loadButton: TagMod =
        (Popup("button", s"Load sequence ${sequenceId.format}")(
          Button(
            size     = Size.Large,
            compact  = true,
            icon     = Some(IconUpload),
            color    = "teal".some,
            disabled = b.state.loading || running,
            loading  = b.state.loading,
            onClickE = load(b, instrument, sequenceId) _
          )
        ): VdomNode).when(isPreview && isLogged)

      val instrumentWithId =
        React.Fragment(
          <.div(SeqexecStyles.activeInstrumentLabel, dispName),
          Label(
            Label.Props(tabTitle,
                        color       = color.some,
                        icon        = icon.some,
                        extraStyles = List(SeqexecStyles.labelPointer))
          )
        )

      val resourceLabels =
        <.div(
          SeqexecStyles.resourceLabels,
          resources.map {
            case (r, s) =>
              val color = s match {
                case ResourceRunOperation.ResourceRunIdle         => "white"
                case ResourceRunOperation.ResourceRunCompleted(_) => "green"
                case ResourceRunOperation.ResourceRunInFlight(_)  => "yellow"
                case ResourceRunOperation.ResourceRunFailed(_)    => "red"
              }
              s match {
                case ResourceRunOperation.ResourceRunInFlight(_) =>
                  Label(
                    Label.Props(r.show,
                                color = color.some,
                                size  = Size.Small,
                                extraStyles = List(
                                  SeqexecStyles.activeResourceLabel))): VdomNode
                case ResourceRunOperation.ResourceRunCompleted(_) =>
                  Label(Label.Props(r.show,
                                    color = color.some,
                                    size  = Size.Small)): VdomNode
                case _ => EmptyVdom
              }
          }.toTagMod
        )

      val instrumentAndResources =
        React.Fragment(
          <.div(SeqexecStyles.instrumentAndResourcesLabel,
                <.div(SeqexecStyles.tabLabel, dispName),
                resourceLabels),
          Label(
            Label.Props(tabTitle,
                        color       = color.some,
                        icon        = icon.some,
                        extraStyles = List(SeqexecStyles.labelPointer))
          )
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
      Callback.when(preview && (id =!= newId || (wasLoading && !isLoading)))(
        f.setState(State(false))
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
