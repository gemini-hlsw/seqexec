// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import scala.scalajs.js
import seqexec.model.enum.{ Instrument, StepType }
import seqexec.model.{ StepState, Step, StandardStep }
import seqexec.web.client.lenses._
import seqexec.web.client.model.Pages.SeqexecPages
import seqexec.web.client.ModelOps._
import seqexec.web.client.circuit.{ClientStatus, StepsTableAndStatusFocus, StepsTableFocus}
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.components.sequence.steps.OffsetFns._
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.semanticui.{Size => SSize}
import react.virtualized._
import web.client.style._
import web.client.table._

object ColWidths {
  val ControlWidth: Int       = 40
  val IdxWidth: Int           = 50
  val StateWidth: Int         = 200
  val StatusWidth: Int        = 100
  val OffsetWidthBase: Int    = 75
  val ExposureWidth: Int      = 75
  val DisperserWidth: Int     = 100
  val ObservingModeWidth: Int = 180
  val FilterWidth: Int        = 100
  val FPUWidth: Int           = 100
  val ObjectTypeWidth: Int    = 75
  val SettingsWidth: Int      = 34
}

/**
  * Container for a table with the steps
  */
object StepsTable {
  val HeightWithOffsets: Int    = 40
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

  final case class Props(router: RouterCtl[SeqexecPages],
                         loggedIn: Boolean,
                         stepsTable: ModelProxy[StepsTableAndStatusFocus],
                         onStepToRun: Int => Callback) {
    def status: ClientStatus           = stepsTable().status
    def steps: Option[StepsTableFocus] = stepsTable().stepsTable
    val stepsList: List[Step]          = steps.map(_.steps).getOrElse(Nil)
    def rowCount: Int                  = stepsList.length

    def rowGetter(idx: Int): StepRow =
      steps.flatMap(_.steps.lift(idx)).fold(StepRow.Zero)(StepRow.apply)

    val configTableState: TableState[StepConfigTable.TableColumn] =
      stepsTable().configTableState
    // Find out if offsets should be displayed
    val offsetsDisplay: OffsetsDisplay = stepsList.offsetsDisplay
    private def showProp(p: InstrumentProperties): Boolean =
      steps.exists(s => s.instrument.displayItems.contains(p))

    val showOffsets: Boolean   = showProp(InstrumentProperties.Offsets)
    val showDisperser: Boolean = showProp(InstrumentProperties.Disperser)
    val showFPU: Boolean       = showProp(InstrumentProperties.FPU)
    val isPreview: Boolean     = steps.map(_.isPreview).getOrElse(false)

    val showObservingMode: Boolean = showProp(
      InstrumentProperties.ObservingMode)
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

  def stepControlRenderer(f: StepsTableFocus,
                          p: Props,
                          recomputeHeightsCB: Int => Callback): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      StepToolsCell(
        StepToolsCell.Props(p.status,
                            f,
                            row.step,
                            rowHeight(p)(row.step.id),
                            recomputeHeightsCB))

  val stepIdRenderer: CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) => StepIdCell(row.step.id)

  def settingsControlRenderer(
      p: Props,
      f: StepsTableFocus): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      SettingsCell(
        SettingsCell.Props(p.router, f.instrument, f.id, row.step.id))

  def stepProgressRenderer(
      f: StepsTableFocus,
      p: Props): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      StepProgressCell(StepProgressCell.Props(p.status, f, row.step))

  def stepStatusRenderer(offsetsDisplay: OffsetsDisplay)
    : CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      OffsetsDisplayCell(OffsetsDisplayCell.Props(offsetsDisplay, row.step))

  def stepDisperserRenderer(
      i: Instrument): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      DisperserCell(DisperserCell.Props(row.step, i))

  def stepExposureRenderer(
      i: Instrument): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      ExposureTimeCell(ExposureTimeCell.Props(row.step, i))

  def stepFilterRenderer(
      i: Instrument): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) => FilterCell(FilterCell.Props(row.step, i))

  def stepFPURenderer(
      i: Instrument): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) => FPUCell(FPUCell.Props(row.step, i))

  val stepObsModeRenderer: CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      ObservingModeCell(ObservingModeCell.Props(row.step))

  def stepObjectTypeRenderer(
      size: SSize): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      ObjectTypeCell(ObjectTypeCell.Props(row.step, size))

  private def stepRowStyle(step: Step): GStyle = step match {
    case s if s.hasError                     => SeqexecStyles.rowError
    case s if s.status === StepState.Running => SeqexecStyles.rowWarning
    case s if s.status === StepState.Paused  => SeqexecStyles.rowNegative
    case s if s.status === StepState.Skipped => SeqexecStyles.rowActive
    case s if s.isFinished                   => SeqexecStyles.rowDone
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
    case _ if displayOffsets(p)               =>
      HeightWithOffsets
    case StepRow(StandardStep(_, _, _, true, _, _, _, _))                      =>
      SeqexecStyles.rowHeight + BreakpointLineHeight
    case _                                                                     =>
      SeqexecStyles.rowHeight
  }

  class Backend {
    private val PhoneCut      = 412
    private val LargePhoneCut = 767

    val idxColumn: Table.ColumnArg =
      Column(
        Column.propsNoFlex(ColWidths.IdxWidth,
                           "idx",
                           label = "Step",
                           className = SeqexecStyles.paddedStepRow.htmlClass,
                           cellRenderer = stepIdRenderer))

    def stateColumn(p: Props, controlWidth: Double): Option[Table.ColumnArg] =
      p.steps.map(
        i =>
          Column(
            Column.propsNoFlex(
              controlWidth,
              "state",
              label = "Control",
              className = SeqexecStyles.paddedStepRow.htmlClass,
              cellRenderer = stepProgressRenderer(i, p))))

    def iconColumn(p: Props): Option[Table.ColumnArg] =
      p.steps.map(
        i =>
          Column(
            Column.propsNoFlex(
              ColWidths.ControlWidth,
              "ctl",
              label = "Icon",
              cellRenderer = stepControlRenderer(i, p, recomputeRowHeightsCB),
              className = SeqexecStyles.controlCellRow.htmlClass,
              headerRenderer = controlHeaderRenderer,
              headerClassName =
                (SeqexecStyles.centeredCell |+| SeqexecStyles.tableHeaderIcons).htmlClass
            )))

    def offsetColumn(p: Props,
                     offsetVisible: Boolean): (Option[Table.ColumnArg], Int) =
      p.offsetsDisplay match {
        case OffsetsDisplay.DisplayOffsets(x) if p.showOffsets =>
          val width = ColWidths.OffsetWidthBase + x
          (Column(
             Column.propsNoFlex(width,
                                "offset",
                                label = "Offset",
                                cellRenderer =
                                  stepStatusRenderer(p.offsetsDisplay))).some
             .filter(_ => offsetVisible),
           width)
        case _ => (None, 0)
      }

    def disperserColumn(p: Props,
                        disperserVisible: Boolean): Option[Table.ColumnArg] =
      for {
        col <- p.steps.map(
                s =>
                  Column(
                    Column.propsNoFlex(
                      ColWidths.DisperserWidth,
                      "disperser",
                      label = "Disperser",
                      className = SeqexecStyles.centeredCell.htmlClass,
                      cellRenderer = stepDisperserRenderer(s.instrument)
                    )))
        if p.showDisperser
        if disperserVisible
      } yield col

    def exposureColumn(p: Props,
                       exposureVisible: Boolean): Option[Table.ColumnArg] =
      for {
        col <- p.steps.map(
                i =>
                  Column(
                    Column.propsNoFlex(
                      ColWidths.ExposureWidth,
                      "exposure",
                      label = "Exposure",
                      className = SeqexecStyles.centeredCell.htmlClass,
                      cellRenderer = stepExposureRenderer(i.instrument)
                    )))
        if exposureVisible
      } yield col

    def fpuColumn(p: Props, fpuVisible: Boolean): Option[Table.ColumnArg] =
      for {
        col <- p.steps.map(
                i =>
                  Column(
                    Column.propsNoFlex(
                      ColWidths.FPUWidth,
                      "fpu",
                      label = "FPU",
                      className = SeqexecStyles.centeredCell.htmlClass,
                      cellRenderer = stepFPURenderer(i.instrument))))
        if p.showFPU
        if fpuVisible
      } yield col

    def observingModeColumn(p: Props): Option[Table.ColumnArg] =
      for {
        col <- p.steps.map(
                i =>
                  Column(
                    Column.propsNoFlex(
                      ColWidths.ObservingModeWidth,
                      "obsMode",
                      label = "Observing Mode",
                      className = SeqexecStyles.centeredCell.htmlClass,
                      cellRenderer = stepObsModeRenderer
                    )))
        if p.showObservingMode
      } yield col

    def filterColumn(p: Props,
                     filterVisible: Boolean): Option[Table.ColumnArg] =
      p.steps
        .map(
          i =>
            Column(Column.propsNoFlex(
              ColWidths.FilterWidth,
              "filter",
              label = "Filter",
              className = SeqexecStyles.centeredCell.htmlClass,
              cellRenderer = stepFilterRenderer(i.instrument)
            )))
        .filter(_ => filterVisible)

    def typeColumn(p: Props, objectSize: SSize): Option[Table.ColumnArg] =
      p.steps.map(
        i =>
          Column(
            Column.propsNoFlex(
              ColWidths.ObjectTypeWidth,
              "type",
              label = "Type",
              className = SeqexecStyles.centeredCell.htmlClass,
              cellRenderer = stepObjectTypeRenderer(objectSize)
            )))

    def settingsColumn(p: Props): Option[Table.ColumnArg] =
      p.steps.map(
        i =>
          Column(
            Column.propsNoFlex(
              ColWidths.SettingsWidth,
              "set",
              label = "",
              cellRenderer = settingsControlRenderer(p, i),
              className = SeqexecStyles.settingsCellRow.htmlClass,
              headerRenderer = settingsHeaderRenderer,
              headerClassName =
                (SeqexecStyles.centeredCell |+| SeqexecStyles.tableHeaderIcons).htmlClass
            )))

    // Columns for the table
    private def columns(p: Props, s: Size): List[Table.ColumnArg] = {
      val (offsetVisible, exposureVisible, disperserVisible, fpuVisible, filterVisible, objectSize) =
        s.width match {
          case w if w < PhoneCut      =>
            (false, false, false, false, false, SSize.Tiny)
          case w if w < LargePhoneCut =>
            (false, true, false, false, false, SSize.Small)
          case _                      =>
            (displayOffsets(p), true, true, true, true, SSize.Small)
        }

      val (offsetCol, offsetWidth)        = offsetColumn(p, offsetVisible)
      val disperserCol                    = disperserColumn(p, disperserVisible)
      val observingModeCol                = observingModeColumn(p)
      val exposureCol                     = exposureColumn(p, exposureVisible)
      val fpuCol: Option[Table.ColumnArg] = fpuColumn(p, fpuVisible)
      val iconCol                         = iconColumn(p)
      val filterCol                       = filterColumn(p, filterVisible)
      val typeCol                         = typeColumn(p, objectSize)
      val settingsCol                     = settingsColumn(p)

      // Let's precisely calculate the width of the control column
      val colsWidth =
        ColWidths.ControlWidth +
          ColWidths.IdxWidth +
          offsetCol.fold(0)(_ => offsetWidth) +
          exposureCol.fold(0)(_ => ColWidths.ExposureWidth) +
          disperserCol.fold(0)(_ => ColWidths.DisperserWidth) +
          filterCol.fold(0)(_ => ColWidths.FilterWidth) +
          fpuCol.fold(0)(_ => ColWidths.FPUWidth) +
          observingModeCol.fold(0)(_ => ColWidths.ObservingModeWidth) +
          ColWidths.ObjectTypeWidth +
          ColWidths.SettingsWidth
      val controlWidth = s.width - colsWidth

      val stateCol = stateColumn(p, controlWidth)

      List(
        iconCol,
        idxColumn.some,
        stateCol,
        offsetCol,
        observingModeCol,
        exposureCol,
        disperserCol,
        filterCol,
        fpuCol,
        typeCol,
        settingsCol
      ).collect { case Some(x) => x }
    }

    def stepsTableProps(p: Props)(size: Size): Table.Props =
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
        headerHeight = SeqexecStyles.headerHeight
      )

    // Create a ref
    private val ref = Ref.toJsComponent(Table.component)

    private def recomputeRowHeightsCB(index: Int): Callback =
      ref.get.flatMapCB(_.raw.recomputeRowsHeightsCB(index))

    def receive(cur: Props, next: Props): Callback = {
      // Recalculate the heights if needed
      val stepsPairs = next.stepsList.zip(cur.stepsList)
      val differentStepsStates: List[Callback] = stepsPairs.collect {
        // if step status changes recalculate
        case (cur, prev) if cur.status =!= prev.status =>
          ref.get.flatMapCB(_.raw.recomputeRowsHeightsCB(cur.id)).toCallback
        // if breakpoint state changes recalculate
        case (cur, prev) if cur.breakpoint =!= prev.breakpoint =>
          ref.get.flatMapCB(_.raw.recomputeRowsHeightsCB(cur.id)).toCallback
      }
      Callback.sequence(differentStepsStates)
    }

    // Wire it up from VDOM
    def render(p: Props): VdomElement = {
      val settingsDisplayed = p.steps.forall(_.stepConfigDisplayed.isDefined)
      val isTall = (p.status.isLogged || settingsDisplayed) && !p.isPreview
      <.div(
        SeqexecStyles.stepsListPane.unless(isTall),
        SeqexecStyles.stepsListPaneWithControls.when(isTall),
        p.steps.whenDefined { tab =>
          tab.stepConfigDisplayed
            .map { i =>
              val steps = p.stepsList.lift(i).getOrElse(Step.Zero)
              AutoSizer(AutoSizer.props(s =>
                StepConfigTable(
                  StepConfigTable.Props(steps, s, p.configTableState))))
            }
            .getOrElse {
              AutoSizer(
                AutoSizer.props(s =>
                  ref.component(stepsTableProps(p)(s))(
                    columns(p, s).map(_.vdomElement): _*)))
            }
            .vdomElement
        }
      )
    }
  }

  private val component = ScalaComponent
    .builder[Props]("Steps")
    .renderBackend[Backend]
    .componentWillReceiveProps(x =>
      x.backend.receive(x.currentProps, x.nextProps))
    .build

  def apply(p: Props): Unmounted[Props, Unit, Backend] = component(p)
}
