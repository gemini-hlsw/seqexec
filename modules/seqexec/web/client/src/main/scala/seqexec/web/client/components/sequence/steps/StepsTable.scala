// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import scala.scalajs.js
import diode.react.ModelProxy
import seqexec.model.Model.{Instrument, StandardStep, Step, StepState, StepType}
import seqexec.model.properties
import seqexec.web.client.lenses._
import seqexec.web.client.model.Pages.SeqexecPages
import seqexec.web.client.circuit.{ClientStatus, StepsTableAndStatusFocus, StepsTableFocus}
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.components.sequence.steps.OffsetFns._
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.semanticui.{Size => SSize}
import web.client.style._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import cats.implicits._
import react.virtualized._
import mouse.boolean._

object ColWidths {
  val ControlWidth: Int = 40
  val IdxWidth: Int = 50
  val StateWidth: Int = 200
  val StatusWidth: Int = 100
  val OffsetWidthBase: Int = 75
  val ExposureWidth: Int = 75
  val DisperserWidth: Int = 100
  val FilterWidth: Int = 100
  val FPUWidth: Int = 100
  val ObjectTypeWidth: Int = 75
  val SettingsWidth: Int = 34
}

/**
  * Container for a table with the steps
  */
object StepsTable {
  val HeightWithOffsets: Int = 40
  val BreakpointLineHeight: Int = 5

  // ScalaJS defined trait
  // scalastyle:off
  trait StepRow extends js.Object {
    var step: Step
  }
  // scalastyle:on
  object StepRow {
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def apply(step: Step): StepRow = {
      val p = (new js.Object).asInstanceOf[StepRow]
      p.step = step
      p
    }

    def unapply(l: StepRow): Option[(Step)] =
      Some((l.step))

    val Zero: StepRow = apply(Step.Zero)
  }

  final case class Props(router: RouterCtl[SeqexecPages], loggedIn: Boolean, stepsTable: ModelProxy[StepsTableAndStatusFocus], onStepToRun: Int => Callback) {
    def status: ClientStatus = stepsTable().status
    def steps: Option[StepsTableFocus] = stepsTable().stepsTable
    val stepsList: List[Step] = steps.map(_.steps).getOrElse(Nil)
    def rowCount: Int = stepsList.length
    def rowGetter(idx: Int): StepRow = steps.flatMap(_.steps.lift(idx)).fold(StepRow.Zero)(StepRow.apply)
    val configTableState: StepConfigTable.TableState = stepsTable().configTableState
    // Find out if offsets should be displayed
    val offsetsDisplay: OffsetsDisplay = stepsList.offsetsDisplay
  }

  val controlHeaderRenderer: HeaderRenderer[js.Object] = (_, _, _, _, _, _) =>
      <.span(
        SeqexecStyles.centeredCell,
        ^.title := "Control",
        IconSettings
      )

  val settingsHeaderRenderer: HeaderRenderer[js.Object] = (_, _, _, _, _, _) =>
      <.span(
        SeqexecStyles.settingsCellHeader,
        ^.title := "Settings",
        IconBrowser
      )

  def stepControlRenderer(f: StepsTableFocus, p: Props, recomputeHeightsCB: Int => Callback): CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    StepToolsCell(StepToolsCell.Props(p.status, f, row.step, rowHeight(p)(row.step.id), recomputeHeightsCB))

  val stepIdRenderer: CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    StepIdCell(row.step.id)

  def settingsControlRenderer(p: Props, f: StepsTableFocus): CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    SettingsCell(SettingsCell.Props(p.router, f.instrument, f.id, row.step.id))

  def stepProgressRenderer(f: StepsTableFocus, p: Props): CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    StepProgressCell(StepProgressCell.Props(p.status, f, row.step))

  def stepStatusRenderer(offsetsDisplay: OffsetsDisplay): CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    OffsetsDisplayCell(OffsetsDisplayCell.Props(offsetsDisplay, row.step))

  def stepDisperserRenderer(i: Instrument): CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    DisperserCell(DisperserCell.Props(row.step, i))

  def stepExposureRenderer(i: Instrument): CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    ExposureTimeCell(ExposureTimeCell.Props(row.step, i))

  def stepFilterRenderer(i: Instrument): CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    FilterCell(FilterCell.Props(row.step, i))

  def stepFPURenderer(i: Instrument): CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    FPUCell(FPUCell.Props(row.step, i))

  def stepObjectTypeRenderer(size: SSize): CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    ObjectTypeCell(ObjectTypeCell.Props(row.step, size))

  private def stepRowStyle(step: Step): GStyle = step match {
    case s if s.hasError                     => SeqexecStyles.rowError
    case s if s.status === StepState.Running => SeqexecStyles.rowWarning
    case s if s.status === StepState.Paused  => SeqexecStyles.rowNegative
    case s if s.status === StepState.Skipped => SeqexecStyles.rowActive
    case s if s.isFinished                   => SeqexecStyles.rowDisabled
    case _                                   => SeqexecStyles.stepRow
  }

  def rowClassName(p: Props)(i: Int): String = ((i, p.rowGetter(i), p.loggedIn) match {
    case (-1, _, _)                                                       =>
      SeqexecStyles.headerRowStyle
    case (_, StepRow(s @ StandardStep(_, _, _, true, _, _, _, _)), true)  =>
      SeqexecStyles.stepRowWithBreakpointAndControl |+| stepRowStyle(s)
    case (_, StepRow(s @ StandardStep(_, _, _, true, _, _, _, _)), false) =>
      SeqexecStyles.stepRowWithBreakpoint |+| stepRowStyle(s)
    case (_, StepRow(s), _)                                               =>
      SeqexecStyles.stepRow |+| stepRowStyle(s)
    case _                                                                =>
      SeqexecStyles.stepRow
  }).htmlClass

  def displayOffsets(p: Props): Boolean =
    p.stepsList.headOption.flatMap(stepTypeO.getOption) match {
      case Some(StepType.Object) => true
      case _                     => false
    }

  def rowHeight(p: Props)(i: Int): Int = p.rowGetter(i) match {
    case StepRow(StandardStep(_, _, _, true, _, _, _, _)) if displayOffsets(p) =>
      HeightWithOffsets + BreakpointLineHeight
    case StepRow(s: Step) if s.status === StepState.Running                    =>
      SeqexecStyles.runningRowHeight
    case _ if displayOffsets(p)                                                =>
      HeightWithOffsets
    case StepRow(StandardStep(_, _, _, true, _, _, _, _))                      =>
      SeqexecStyles.rowHeight + BreakpointLineHeight
    case _                                                                     =>
      SeqexecStyles.rowHeight
  }

  class Backend {
    private val PhoneCut = 412
    private val LargePhoneCut = 767

    // Columns for the table
    private def columns(p: Props, s: Size): List[Table.ColumnArg] = {
      val (offsetVisible, exposureVisible, disperserVisible, fpuVisible, filterVisible, objectSize) = s.width match {
        case w if w < PhoneCut      => (false, false, false, false, false, SSize.Tiny)
        case w if w < LargePhoneCut => (false, true, false, false, false, SSize.Small)
        case _                      => (displayOffsets(p), true, true, true, true, SSize.Small)
      }

      val (offsetColumn, offsetWidth) =
        p.offsetsDisplay match {
          case OffsetsDisplay.DisplayOffsets(x) =>
            val width = ColWidths.OffsetWidthBase + x
            (Column(Column.props(width, "offset", label = "Offset", flexShrink = 0, flexGrow = 0, disableSort = true, cellRenderer = stepStatusRenderer(p.offsetsDisplay))).some, width)
          case _ => (None, 0)
        }
      val disperserColumn =
        if (p.steps.exists(s => properties.instrumentProperties(s.instrument).contains(properties.Disperser)))
          ((i: StepsTableFocus) => Column(Column.props(ColWidths.DisperserWidth, "disperser", label = "Disperser", flexShrink = 0, flexGrow = 0, disableSort = true, className = SeqexecStyles.centeredCell.htmlClass, cellRenderer = stepDisperserRenderer(i.instrument)))).some
        else
          None

      // Let's precisely calculate the width of the control column
      val controlWidth = s.width - (ColWidths.ControlWidth + ColWidths.IdxWidth + offsetVisible.fold(offsetWidth, 0) + exposureVisible.fold(ColWidths.ExposureWidth, 0) + disperserVisible.fold(disperserColumn.fold(0)(_ => ColWidths.DisperserWidth), 0) + filterVisible.fold(ColWidths.FilterWidth, 0) + fpuVisible.fold(ColWidths.FPUWidth, 0) + ColWidths.ObjectTypeWidth + ColWidths.SettingsWidth)

      List(
        p.steps.map(i => Column(Column.props(ColWidths.ControlWidth, "ctl", label = "Icon", disableSort = true, cellRenderer = stepControlRenderer(i, p, recomputeRowHeightsCB), flexShrink = 0, className = SeqexecStyles.controlCellRow.htmlClass, headerRenderer = controlHeaderRenderer, headerClassName = (SeqexecStyles.centeredCell |+| SeqexecStyles.tableHeaderIcons).htmlClass))),
        Column(Column.props(ColWidths.IdxWidth, "idx", label = "Step", flexShrink = 0, flexGrow = 0, disableSort = true, className = SeqexecStyles.paddedStepRow.htmlClass, cellRenderer = stepIdRenderer)).some,
        p.steps.map(i => Column(Column.props(controlWidth, "state", flexGrow = 0, flexShrink = 0, label = "Control", disableSort = true, className = SeqexecStyles.paddedStepRow.htmlClass, cellRenderer = stepProgressRenderer(i, p)))),
        offsetColumn.filter(_ => offsetVisible),
        p.steps.map(i => Column(Column.props(ColWidths.ExposureWidth, "exposure", flexShrink = 0, flexGrow = 0, label = "Exposure", disableSort = true, className = SeqexecStyles.centeredCell.htmlClass, cellRenderer = stepExposureRenderer(i.instrument)))).filter(_ => exposureVisible),
        disperserColumn.filter(_ => disperserVisible).flatMap(c => p.steps.map(c)),
        p.steps.map(i => Column(Column.props(ColWidths.FilterWidth, "filter", flexGrow = 0, flexShrink = 0, label = "Filter", disableSort = true, className = SeqexecStyles.centeredCell.htmlClass, cellRenderer = stepFilterRenderer(i.instrument)))).filter(_ => filterVisible),
        p.steps.map(i => Column(Column.props(ColWidths.FPUWidth, "fpu", flexShrink = 0, flexGrow = 0, label = "FPU", disableSort = true, className = SeqexecStyles.centeredCell.htmlClass, cellRenderer = stepFPURenderer(i.instrument)))).filter(_ => fpuVisible),
        p.steps.map(i => Column(Column.props(ColWidths.ObjectTypeWidth, "type", flexShrink = 0, flexGrow = 0, label = "Type", disableSort = true, className = SeqexecStyles.centeredCell.htmlClass, cellRenderer = stepObjectTypeRenderer(objectSize)))),
        p.steps.map(i => Column(Column.props(ColWidths.SettingsWidth, "set", flexShrink = 0, flexGrow = 0, label = "", disableSort = true, cellRenderer = settingsControlRenderer(p, i), className = SeqexecStyles.settingsCellRow.htmlClass, headerRenderer = settingsHeaderRenderer, headerClassName = (SeqexecStyles.centeredCell |+| SeqexecStyles.tableHeaderIcons).htmlClass)))
      ).collect { case Some(x) => x }
    }

    def stepsTableProps(p: Props)(size: Size): Table.Props = {
      Table.props(
        disableHeader = false,
        noRowsRenderer = () =>
          <.div(
            ^.cls := "ui center aligned segment noRows",
            ^.height := size.height.px,
            "No Steps"
          ),
        overscanRowCount = SeqexecStyles.overscanRowCount,
        height = size.height.toInt,
        rowCount = p.rowCount,
        rowHeight = rowHeight(p) _,
        rowClassName = rowClassName(p) _,
        width = size.width.toInt,
        rowGetter = p.rowGetter _,
        scrollTop = 0,
        headerClassName = SeqexecStyles.tableHeader.htmlClass,
        headerHeight = SeqexecStyles.headerHeight)
    }

    // Create a ref
    private val ref = Ref.toJsComponent(Table.component)

    private def recomputeRowHeightsCB(index: Int): Callback = ref.get.flatMapCB(_.raw.recomputeRowsHeightsCB(index))

    def receive(cur: Props, next: Props): Callback = {
      // Recalculate the heights if needed
      val stepsPairs = next.stepsList.zip(cur.stepsList)
      val differentStepsStates: List[Callback] = stepsPairs.collect {
        // if step status changes recalculate
        case (cur, prev) if cur.status =!= prev.status         => ref.get.flatMapCB(_.raw.recomputeRowsHeightsCB(cur.id)).toCallback
        // if breakpoint state changes recalculate
        case (cur, prev) if cur.breakpoint =!= prev.breakpoint => ref.get.flatMapCB(_.raw.recomputeRowsHeightsCB(cur.id)).toCallback
      }
      Callback.sequence(differentStepsStates)
    }

    // Wire it up from VDOM
    def render(p: Props): VdomElement = {
      val settingsDisplayed = p.steps.forall(_.stepConfigDisplayed.isDefined)
      <.div(
        SeqexecStyles.stepsListPane.unless(p.status.isLogged || settingsDisplayed),
        SeqexecStyles.stepsListPaneWithControls.when(p.status.isLogged || settingsDisplayed),
        p.steps.whenDefined { tab =>
          tab.stepConfigDisplayed.map { i =>
            val steps = p.stepsList.lift(i).getOrElse(Step.Zero)
            AutoSizer(AutoSizer.props(s => StepConfigTable(StepConfigTable.Props(steps, s, p.configTableState))))
          }.getOrElse {
            AutoSizer(AutoSizer.props(s => ref.component(stepsTableProps(p)(s))(columns(p, s).map(_.vdomElement): _*)))
          }.vdomElement
        }
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("Steps")
    .renderBackend[Backend]
    .componentWillReceiveProps(x => x.backend.receive(x.currentProps, x.nextProps))
    .build

  def apply(p: Props): Unmounted[Props, Unit, Backend] = component(p)
}
