// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.steps

import scala.scalajs.js
import diode.react.ModelProxy
import edu.gemini.seqexec.model.Model.{Instrument, StandardStep, Step, StepState}
import edu.gemini.seqexec.web.client.model.Pages.SeqexecPages
import edu.gemini.seqexec.web.client.circuit.{ClientStatus, StepsTableFocus}
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.components.sequence.steps.OffsetFns._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
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
  val IdxWidth: Int = 40
  val StateWidth: Int = 200
  val StatusWidth: Int = 100
  val OffsetWidthBase: Int = 85
  val GuidingWidth: Int = 63
  val ExposureWidth: Int = 80
  val FilterWidth: Int = 150
  val FPUWidth: Int = 150
  val ObjectTypeWidth: Int = 75
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

  final case class Props(router: RouterCtl[SeqexecPages], stepsTable: ModelProxy[(ClientStatus, Option[StepsTableFocus])], onStepToRun: Int => Callback) {
    def status: ClientStatus = stepsTable()._1
    def steps: Option[StepsTableFocus] = stepsTable()._2
    val stepsList: List[Step] = ~steps.map(_.steps)
    def rowCount: Int = stepsList.length
    def rowGetter(idx: Int): StepRow = steps.flatMap(_.steps.index(idx)).fold(StepRow.Zero)(StepRow.apply)
    // Find out if offsets should be displayed
    val offsetsDisplay: OffsetsDisplay = stepsList.offsetsDisplay
  }

  val controlHeaderRenderer: HeaderRenderer[js.Object] = (_, _, _, _, _, _) =>
      <.span(
        SeqexecStyles.centeredCell,
        SeqexecStyles.controlCellHeader,
        ^.title := "Control",
        IconSettings
      )

  def stepControlRenderer(f: StepsTableFocus, p: Props, recomputeHeightsCB: Int => Callback): CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    StepToolsCell(StepToolsCell.Props(p.status, f, row.step, rowHeight(p)(row.step.id), recomputeHeightsCB))

  val stepIdRenderer: CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    StepIdCell(row.step.id)

  def stepProgressRenderer(f: StepsTableFocus, p: Props): CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    StepProgressCell(StepProgressCell.Props(p.status, f, row.step))

  def stepStatusRenderer(offsetsDisplay: OffsetsDisplay): CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    OffsetsDisplayCell(OffsetsDisplayCell.Props(offsetsDisplay, row.step))

  val stepGuidingRenderer: CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    GuidingCell(GuidingCell.Props(row.step))

  def stepExposureRenderer(i: Instrument): CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    ExposureTimeCell(ExposureTimeCell.Props(row.step, i))

  def stepFilterRenderer(i: Instrument): CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    FilterCell(FilterCell.Props(row.step, i))

  def stepFPURenderer(i: Instrument): CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    FPUCell(FPUCell.Props(row.step, i))

  val stepObjectTypeRenderer: CellRenderer[js.Object, js.Object, StepRow] = (_, _, _, row: StepRow, _) =>
    ObjectTypeCell(row.step)

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

  def rowHeight(p: Props)(i: Int): Int = (p.rowGetter(i), p.offsetsDisplay) match {
    case (StepRow(StandardStep(_, _, _, true, _, _, _, _)), OffsetsDisplay.DisplayOffsets(_)) =>
      HeightWithOffsets + BreakpointLineHeight
    case (_, OffsetsDisplay.DisplayOffsets(_))                                                =>
      HeightWithOffsets
    case (StepRow(s: Step), _) if s.status === StepState.Running                              =>
      SeqexecStyles.runningRowHeight
    case (StepRow(StandardStep(_, _, _, true, _, _, _, _)), _)                                =>
      SeqexecStyles.rowHeight + BreakpointLineHeight
    case _ =>
      SeqexecStyles.rowHeight
  }

  class Backend {
    // Columns for the table
    private def columns(p: Props): List[Table.ColumnArg] = {
      val offsetColumn =
        p.offsetsDisplay match {
          case OffsetsDisplay.DisplayOffsets(x) =>
            Column(Column.props(ColWidths.OffsetWidthBase + x, "offset", label = "Offset", flexShrink = 0, disableSort = true, cellRenderer = stepStatusRenderer(p.offsetsDisplay))).some
          case _ => None
        }
        List(
          p.steps.map(i => Column(Column.props(ColWidths.ControlWidth, "ctl", label = "Icon", disableSort = true, cellRenderer = stepControlRenderer(i, p, recomputeRowHeightsCB), flexShrink = 0, className = SeqexecStyles.controlCellRow.htmlClass, headerRenderer = controlHeaderRenderer))),
          Column(Column.props(ColWidths.IdxWidth, "idx", label = "Step", disableSort = true, flexShrink = 0, cellRenderer = stepIdRenderer)).some,
          p.steps.map(i => Column(Column.props(ColWidths.StateWidth, "state", label = "Control", flexShrink = 1, flexGrow = 1, disableSort = true, cellRenderer = stepProgressRenderer(i, p)))),
          offsetColumn,
          Column(Column.props(ColWidths.GuidingWidth, "guiding", label = "Guiding", flexShrink = 0, disableSort = true, cellRenderer = stepGuidingRenderer, className = SeqexecStyles.centeredCell.htmlClass)).some,
          p.steps.map(i => Column(Column.props(ColWidths.ExposureWidth, "exposure", label = "Exposure", flexShrink = 0, disableSort = true, className = SeqexecStyles.centeredCell.htmlClass, cellRenderer = stepExposureRenderer(i.instrument)))),
          p.steps.map(i => Column(Column.props(ColWidths.FilterWidth, "filter", label = "Filter", flexShrink = 0, disableSort = true, cellRenderer = stepFilterRenderer(i.instrument)))),
          p.steps.map(i => Column(Column.props(ColWidths.FPUWidth, "fpu", label = "FPU", flexShrink = 0, disableSort = true, cellRenderer = stepFPURenderer(i.instrument)))),
          p.steps.map(i => Column(Column.props(ColWidths.ObjectTypeWidth, "type", label = "Type", flexShrink = 0, disableSort = true, className = SeqexecStyles.rightCell.htmlClass, cellRenderer = stepObjectTypeRenderer)))
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
    def render(p: Props): VdomElement =
      <.div(
        SeqexecStyles.stepsListPane.unless(p.status.isLogged),
        SeqexecStyles.stepsListPaneWithControls.when(p.status.isLogged),
        p.steps.whenDefined { tab =>
          tab.stepConfigDisplayed.map { i =>
            <.div("CONFIG")
          }.getOrElse {
            AutoSizer(AutoSizer.props(s => ref.component(stepsTableProps(p)(s))(columns(p).map(_.vdomElement): _*)))
          }
        }
      )
  }

  private val component = ScalaComponent.builder[Props]("Steps")
    .renderBackend[Backend]
    .componentWillReceiveProps(x => x.backend.receive(x.currentProps, x.nextProps))
    .build

  def apply(p: Props): Unmounted[Props, Unit, Backend] = component(p)
}
