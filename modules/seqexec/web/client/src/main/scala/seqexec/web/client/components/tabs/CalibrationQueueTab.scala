// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.tabs

import cats.implicits._
import gem.Observation
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react._
import monocle.macros.Lenses
import seqexec.model.enum.BatchExecState
import seqexec.model.CalibrationQueueId
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.actions.RequestAddSeqCal
import seqexec.web.client.model.Pages._
import seqexec.web.client.model.CalibrationQueueTabActive
import seqexec.web.client.model.TabSelected
import seqexec.web.client.semanticui._
import seqexec.web.client.semanticui.elements.label.Label
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.reusability._
import web.client.style._

object CalibrationQueueTab {
  type Backend = RenderScope[Props, State, Unit]

  final case class Props(router: RouterCtl[SeqexecPages],
                         tab:    CalibrationQueueTabActive)

  @Lenses
  final case class State(draggingOver: Option[String]) {
    val onDrag: Boolean = draggingOver.isDefined
  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object State

  implicit val propsReuse: Reusability[Props] =
    Reusability.by(x => (x.tab.active, x.tab.calibrationTab.state))
  implicit val stateReuse: Reusability[State] = Reusability.derive[State]

  private val ST = ReactS.Fix[State]

  def showCalibrationQueue(p: Props, page: SeqexecPages)(
    e:                        ReactEvent): Callback =
    // prevent default to avoid the link jumping
    e.preventDefaultCB *>
      // Request to display the selected sequence
      p.router
        .setUrlAndDispatchCB(page)
        .unless(p.tab.active === TabSelected.Selected) *>
      Callback.empty

  def addToQueueE(e: ReactDragEvent): Callback =
    e.preventDefaultCB *>
      Option(e.dataTransfer.getData("text/plain"))
        .flatMap(Observation.Id.fromString)
        .map { id =>
          SeqexecCircuit.dispatchCB(RequestAddSeqCal(CalibrationQueueId, id))
        }
        .getOrEmpty

  private def onDragEnter(e: ReactDragEvent) =
    ST.mod(State.draggingOver.set(Option(e.dataTransfer.getData("text/plain"))))
      .liftCB

  private def onDrop(e: ReactDragEvent) =
    ST.retM(addToQueueE(e)) *>
      onDragEnd

  private def onDragEnd =
    ST.mod(State.draggingOver.set(none)).liftCB

  private def linkTo(b: Backend, page: SeqexecPages)(mod: TagMod*) = {
    val p      = b.props
    val active = p.tab.active

    <.a(
      ^.href := p.router.urlFor(page).value,
      ^.onClick ==> showCalibrationQueue(p, page),
      ^.cls := "item",
      ^.classSet(
        "active" -> (active === TabSelected.Selected)
      ),
      SeqexecStyles.tab,
      dataTab := "daycalqueue",
      SeqexecStyles.inactiveTabContent.when(active === TabSelected.Background),
      SeqexecStyles.activeTabContent
        .when(active === TabSelected.Selected)
        .unless(b.state.onDrag),
      SeqexecStyles.dropOnTab.when(b.state.onDrag),
      ^.onDragOver ==> { (e: ReactDragEvent) =>
        e.preventDefaultCB *> Callback { e.dataTransfer.dropEffect = "copy" }
      },
      ^.onDragEnter ==> b.runStateFn(onDragEnter),
      ^.onDrop ==> b.runStateFn(onDrop),
      ^.onDragEnd --> b.runState(onDragEnd),
      ^.onDragLeave --> b.runState(onDragEnd),
      mod.toTagMod
    )
  }

  private val component = ScalaComponent
    .builder[Props]("CalibrationQueueTab")
    .initialState(State(None))
    .render { b =>
      val tab = b.props.tab
      val icon = tab.calibrationTab.state match {
        case BatchExecState.Running =>
          IconCircleNotched.copyIcon(loading = true)
        case BatchExecState.Completed => IconCheckmark
        case _                        => IconSelectedRadio
      }

      val color = tab.calibrationTab.state match {
        case BatchExecState.Running   => "orange"
        case BatchExecState.Completed => "green"
        case _                        => "grey"
      }

      val tabContent: VdomNode =
        <.div(
          SeqexecStyles.tabLabel,
          <.div(SeqexecStyles.activeInstrumentLabel, "Daytime Queue"),
          Label(
            Label.Props(tab.calibrationTab.state.show,
                        color       = color.some,
                        icon        = icon.some,
                        extraStyles = List(SeqexecStyles.labelPointer)))
        )

      linkTo(b, CalibrationQueuePage)(tabContent)
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, State, Unit] = component(p)
}
