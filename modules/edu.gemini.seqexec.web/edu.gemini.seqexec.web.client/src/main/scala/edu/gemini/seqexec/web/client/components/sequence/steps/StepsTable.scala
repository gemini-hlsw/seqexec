// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.steps

import scala.scalajs.js
import diode.react.ModelProxy
import edu.gemini.seqexec.model.Model.{Instrument, StandardStep, Step, StepState, StepType}
import edu.gemini.seqexec.model.properties
import edu.gemini.seqexec.web.client.lenses._
import edu.gemini.seqexec.web.client.model.Pages.SeqexecPages
import edu.gemini.seqexec.web.client.circuit.{ClientStatus, StepsTableFocus}
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.components.sequence.steps.OffsetFns._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.semanticui.{Size => SSize}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._

import scalacss.ScalaCssReact._
import scalacss._
import scalaz.std.AllInstances._
import scalaz.syntax.foldable._
import scalaz.syntax.equal._
import scalaz.syntax.std.option._
import react.virtualized._

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
  private val CssSettings = scalacss.devOrProdDefaults
  import CssSettings._

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

  final case class Props(router: RouterCtl[SeqexecPages], stepsTable: ModelProxy[StepsTableAndStatusFocus], onStepToRun: Int => Callback) {
    def status: ClientStatus = stepsTable().status
    def steps: Option[StepsTableFocus] = stepsTable().stepsTable
    val stepsList: List[Step] = ~steps.map(_.steps)
    def rowCount: Int = stepsList.length
    def rowGetter(idx: Int): StepRow = steps.flatMap(_.steps.index(idx)).fold(StepRow.Zero)(StepRow.apply)
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

  private def stepRowStyle(step: Step): StyleA = step match {
    case s if s.hasError                     => SeqexecStyles.rowError
    case s if s.status === StepState.Running => SeqexecStyles.rowWarning
    case s if s.status === StepState.Paused  => SeqexecStyles.rowNegative
    case s if s.status === StepState.Skipped => SeqexecStyles.rowActive
    case s if s.isFinished                   => SeqexecStyles.rowDisabled
    case _                                   => SeqexecStyles.rowNone
  }

  def rowClassName(p: Props)(i: Int): String = ((i, p.rowGetter(i)) match {
    case (-1, _)                                                   =>
      SeqexecStyles.headerRowStyle
    case (_, StepRow(s @ StandardStep(_, _, _, true, _, _, _, _))) =>
      SeqexecStyles.stepRowWithBreakpoint + stepRowStyle(s)
    case (_, StepRow(s))                                           =>
      SeqexecStyles.stepRow + stepRowStyle(s)
    case _                                                         =>
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
        case w if w < PhoneCut => (false, false, false, false, false, SSize.Tiny)
        case w if w < LargePhoneCut => (false, true, false, false, false, SSize.Small)
        case _ => (displayOffsets(p), true, true, true, true, SSize.Small)
      }
      val offsetColumn =
        p.offsetsDisplay match {
          case OffsetsDisplay.DisplayOffsets(x) =>
            Column(Column.props(ColWidths.OffsetWidthBase + x, "offset", label = "Offset", flexShrink = 0, disableSort = true, cellRenderer = stepStatusRenderer(p.offsetsDisplay))).some
          case _ => None
        }
      val disperserColumn =
        if (p.steps.exists(s => properties.instrumentProperties(s.instrument).contains(properties.Disperser)))
          ((i: StepsTableFocus) => Column(Column.props(ColWidths.DisperserWidth, "disperser", label = "Disperser", flexShrink = 0, flexGrow = 1, disableSort = true, className = SeqexecStyles.centeredCell.htmlClass, cellRenderer = stepDisperserRenderer(i.instrument)))).some
        else
          None
      List(
        p.steps.map(i => Column(Column.props(ColWidths.ControlWidth, "ctl", label = "Icon", disableSort = true, cellRenderer = stepControlRenderer(i, p, recomputeRowHeightsCB), flexShrink = 0, className = SeqexecStyles.controlCellRow.htmlClass, headerRenderer = controlHeaderRenderer))),
        Column(Column.props(ColWidths.IdxWidth, "idx", label = "Step", disableSort = true, flexShrink = 0, cellRenderer = stepIdRenderer)).some,
        p.steps.map(i => Column(Column.props(ColWidths.StateWidth, "state", label = "Control", flexShrink = 1, flexGrow = 4, disableSort = true, cellRenderer = stepProgressRenderer(i, p)))),
        offsetColumn.filter(_ => offsetVisible),
        p.steps.map(i => Column(Column.props(ColWidths.ExposureWidth, "exposure", label = "Exposure", flexShrink = 0, disableSort = true, className = SeqexecStyles.centeredCell.htmlClass, cellRenderer = stepExposureRenderer(i.instrument)))).filter(_ => exposureVisible),
        disperserColumn.filter(_ => disperserVisible).flatMap(c => p.steps.map(c)),
        p.steps.map(i => Column(Column.props(ColWidths.FilterWidth, "filter", label = "Filter", flexShrink = 0, flexGrow = 1, disableSort = true, className = SeqexecStyles.centeredCell.htmlClass, cellRenderer = stepFilterRenderer(i.instrument)))).filter(_ => filterVisible),
        p.steps.map(i => Column(Column.props(ColWidths.FPUWidth, "fpu", label = "FPU", flexShrink = 4, flexGrow = 1, disableSort = true, className = SeqexecStyles.centeredCell.htmlClass, cellRenderer = stepFPURenderer(i.instrument)))).filter(_ => fpuVisible),
        p.steps.map(i => Column(Column.props(ColWidths.ObjectTypeWidth, "type", label = "Type", flexShrink = 3, disableSort = true, className = SeqexecStyles.centeredCell.htmlClass, cellRenderer = stepObjectTypeRenderer(objectSize)))),
        p.steps.map(i => Column(Column.props(ColWidths.SettingsWidth, "set", label = "", disableSort = true, cellRenderer = settingsControlRenderer(p, i), flexShrink = 0, className = SeqexecStyles.settingsCellRow.htmlClass, headerRenderer = settingsHeaderRenderer)))
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
    private val ref = JsComponent.mutableRefTo(Table.component)
    // Keep it lazy or it won't be properly initialized
    private lazy val tableRef = ref.value.raw

    private def recomputeRowHeightsCB(index: Int): Callback = tableRef.recomputeRowsHeightsCB(index)

    def receive(cur: Props, next: Props): Callback = {
      // Recalculate the heights if needed
      val stepsPairs = next.stepsList.zip(cur.stepsList)
      val differentStepsStates = stepsPairs.collect {
        case (cur, prev) if cur.status =/= prev.status => Callback.log(cur.id) >> tableRef.recomputeRowsHeightsCB(cur.id)
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
            val steps = p.stepsList.index(i).getOrElse(Step.Zero)
            AutoSizer(AutoSizer.props(s => StepConfigTable(StepConfigTable.Props(steps, s))))
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
