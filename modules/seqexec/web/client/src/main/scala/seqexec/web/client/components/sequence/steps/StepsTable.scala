// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.Eq
import cats.data.NonEmptyList
import cats.implicits._
import gem.Observation
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.raw.JsNumber
import monocle.Lens
import monocle.macros.GenLens
import scala.scalajs.js
import scala.math.min
import seqexec.model.enum.Instrument
import seqexec.model.enum.StepType
import seqexec.model.StepState
import seqexec.model.Step
import seqexec.model.StepId
import seqexec.model.StandardStep
import seqexec.web.client.model.lenses._
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.model.TabOperations
import seqexec.web.client.model.Pages.SeqexecPages
import seqexec.web.client.model.ModelOps._
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.circuit.StepsTableAndStatusFocus
import seqexec.web.client.circuit.StepsTableFocus
import seqexec.web.client.actions.UpdateStepTableState
import seqexec.web.client.actions.UpdateSelectedStep
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.components.sequence.steps.OffsetFns._
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.semanticui.{ Size => SSize }
import seqexec.web.client.reusability._
import react.virtualized._
import web.client.style._
import web.client.table._
import web.client.utils._

object ColWidths {
  val ControlWidth: Double       = 40
  val IdxWidth: Double           = 50
  val StateWidth: Double         = 200
  val StatusWidth: Double        = 100
  val OffsetWidthBase: Double    = 75
  val ExposureWidth: Double      = 75
  val DisperserWidth: Double     = 100
  val ObservingModeWidth: Double = 180
  val FilterWidth: Double        = 100
  val FPUWidth: Double           = 100
  val ObjectTypeWidth: Double    = 75
  val SettingsWidth: Double      = 34
}

/**
  * Container for a table with the steps
  */
object StepsTable {
  type Backend = RenderScope[Props, State, Unit]

  sealed trait TableColumn extends Product with Serializable
  case object IconColumn extends TableColumn

  object TableColumn {
    implicit val equal: Eq[TableColumn] = Eq.fromUniversalEquals
  }

  val IconColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    IconColumn,
    name    = "status",
    label   = "",
    visible = true,
    FixedColumnWidth.unsafeFromDouble(ColWidths.ControlWidth))

  val all: NonEmptyList[ColumnMeta[TableColumn]] =
    NonEmptyList.of(IconColumnMeta)

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

  final case class Props(router:      RouterCtl[SeqexecPages],
                         canOperate:  Boolean,
                         stepsTable:  StepsTableAndStatusFocus,
                         onStepToRun: Int => Callback) {
    val status: ClientStatus                        = stepsTable.status
    val steps: Option[StepsTableFocus]              = stepsTable.stepsTable
    val obsId: Option[Observation.Id]               = steps.map(_.id)
    val tableState: Option[TableState[TableColumn]] = steps.map(_.tableState)
    val stepsList: List[Step]                       = steps.foldMap(_.steps)
    val selectedStep: Option[StepId]                = steps.flatMap(_.selectedStep)
    val rowCount: Int                               = stepsList.length
    val nextStepToRun: Int                          = steps.foldMap(_.nextStepToRun).getOrElse(0)
    val tabOperations: TabOperations =
      steps.map(_.tabOperations).getOrElse(TabOperations.Default)
    val showDisperser: Boolean    = showProp(InstrumentProperties.Disperser)
    val showExposure: Boolean     = showProp(InstrumentProperties.Exposure)
    val showFilter: Boolean       = showProp(InstrumentProperties.Filter)
    val showFPU: Boolean          = showProp(InstrumentProperties.FPU)
    val isPreview: Boolean        = steps.map(_.isPreview).getOrElse(false)
    val canSetBreakpoint: Boolean = canOperate && !isPreview
    val showObservingMode: Boolean =
      showProp(InstrumentProperties.ObservingMode)

    def rowGetter(idx: Int): StepRow =
      steps.flatMap(_.steps.lift(idx)).fold(StepRow.Zero)(StepRow.apply)

    def canControlSubsystems(idx: StepId): Boolean =
      !rowGetter(idx).step.isFinished && canOperate && !isPreview

    val configTableState: TableState[StepConfigTable.TableColumn] =
      stepsTable.configTableState
    // Find out if offsets should be displayed
    val offsetsDisplay: OffsetsDisplay = stepsList.offsetsDisplay
    private def showProp(p: InstrumentProperties): Boolean =
      steps.exists(s => s.instrument.displayItems.contains(p))

    val showOffsets: Boolean =
      stepsList.headOption.flatMap(stepTypeO.getOption) match {
        case Some(StepType.Object) => showProp(InstrumentProperties.Offsets)
        case _                     => false
      }

    val startState: State =
      tableState
        .map(s =>
          State.InitialState.copy(tableState = s, selected = selectedStep))
        .getOrElse(State.InitialState)
  }

  final case class State(
    tableState:      TableState[TableColumn],
    breakpointHover: Option[Int],
    selected:        Option[StepId]) // We have this on the step too but this allows faster rerendering

  object State {

    val tableState: Lens[State, TableState[TableColumn]] =
      GenLens[State](_.tableState)

    val breakpointHover: Lens[State, Option[Int]] =
      GenLens[State](_.breakpointHover)

    val selected: Lens[State, Option[StepId]] =
      GenLens[State](_.selected)

    val scrollPosition: Lens[State, JsNumber] =
      tableState ^|-> TableState.scrollPosition[TableColumn]

    val userModified: Lens[State, UserModified] =
      tableState ^|-> TableState.userModified[TableColumn]

    val InitialTableState: TableState[TableColumn] =
      TableState(NotModified, 0, all)

    val InitialState: State = State(InitialTableState, None, None)
  }

  implicit val propsReuse: Reusability[Props] =
    Reusability.by(x => (x.canOperate, x.stepsTable))
  implicit val tcReuse: Reusability[TableColumn] = Reusability.byRef
  implicit val stateReuse: Reusability[State] =
    Reusability.by(x => (x.tableState, x.breakpointHover, x.selected))

  val controlHeaderRenderer: HeaderRenderer[js.Object] = (_, _, _, _, _, _) =>
    <.span(
      ^.title := "Control",
      IconSettings
  )

  val settingsHeaderRenderer: HeaderRenderer[js.Object] = (_, _, _, _, _, _) =>
    <.span(
      ^.title := "Settings",
      IconBrowser
  )

  private def firstRunnableIndex(l: List[Step]): Int =
    l.zipWithIndex.find(!_._1.isFinished).map(_._2).getOrElse(l.length)

  def stepControlRenderer(
    f:                       StepsTableFocus,
    b:                       Backend,
    rowBreakpointHoverOnCB:  Int => Callback,
    rowBreakpointHoverOffCB: Int => Callback,
    recomputeHeightsCB:      Int => Callback
  ): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      StepToolsCell(
        StepToolsCell.Props(
          b.props.status,
          row.step,
          rowHeight(b)(row.step.id),
          f.isPreview,
          f.nextStepToRun,
          f.id,
          firstRunnableIndex(f.steps),
          rowBreakpointHoverOnCB,
          rowBreakpointHoverOffCB,
          recomputeHeightsCB
        ))

  val stepIdRenderer: CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) => StepIdCell(row.step.id)

  def settingsControlRenderer(
    p: Props,
    f: StepsTableFocus
  ): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      SettingsCell(
        SettingsCell
          .Props(p.router, f.instrument, f.id, row.step.id, p.isPreview))

  def stepProgressRenderer(
    f: StepsTableFocus,
    b: Backend
  ): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      StepProgressCell(
        StepProgressCell.Props(b.props.status,
                               f.instrument,
                               f.id,
                               f.state,
                               row.step,
                               b.state.selected,
                               b.props.isPreview,
                               b.props.tabOperations.resourceRunRequested))

  def stepStatusRenderer(
    offsetsDisplay: OffsetsDisplay
  ): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      OffsetsDisplayCell(OffsetsDisplayCell.Props(offsetsDisplay, row.step))

  def stepDisperserRenderer(
    i: Instrument
  ): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      DisperserCell(DisperserCell.Props(row.step, i))

  def stepExposureRenderer(
    i: Instrument
  ): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      ExposureTimeCell(ExposureTimeCell.Props(row.step, i))

  def stepFilterRenderer(
    i: Instrument
  ): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) => FilterCell(FilterCell.Props(row.step, i))

  def stepFPURenderer(
    i: Instrument
  ): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) => FPUCell(FPUCell.Props(row.step, i))

  val stepObsModeRenderer: CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      ObservingModeCell(ObservingModeCell.Props(row.step))

  def stepObjectTypeRenderer(
    size: SSize
  ): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      ObjectTypeCell(ObjectTypeCell.Props(row.step, size))

  private def stepRowStyle(step: Step): GStyle = step match {
    case s if s.hasError                       => SeqexecStyles.rowError
    case s if s.status === StepState.Running   => SeqexecStyles.rowWarning
    case s if s.status === StepState.Paused    => SeqexecStyles.rowNegative
    case s if s.status === StepState.Completed => SeqexecStyles.rowDone
    case s if s.status === StepState.Skipped   => SeqexecStyles.rowActive
    case s if s.isFinished                     => SeqexecStyles.rowDone
    case _                                     => SeqexecStyles.stepRow
  }

  /**
    * Class for the row depends on properties
    */
  def rowClassName(b: Backend)(i: Int): String =
    ((i,
      b.props.rowGetter(i),
      b.props.canSetBreakpoint,
      b.state.breakpointHover) match {
      case (-1, _, _, _) =>
        // Header
        SeqexecStyles.headerRowStyle
      case (_, StepRow(s @ StandardStep(_, _, _, true, _, _, _, _)), true, _) =>
        // row with control elements and breakpoint
        SeqexecStyles.stepRowWithBreakpointAndControl |+| stepRowStyle(s)
      case (_,
            StepRow(s @ StandardStep(_, _, _, true, _, _, _, _)),
            false,
            _) =>
        // row with breakpoint
        SeqexecStyles.stepRowWithBreakpoint |+| stepRowStyle(s)
      case (j,
            StepRow(s @ StandardStep(_, _, _, false, _, _, _, _)),
            _,
            Some(k)) if j === k =>
        // row with breakpoint and hover
        SeqexecStyles.stepRowWithBreakpointHover |+| stepRowStyle(s)
      case (_, StepRow(s), _, _) =>
        // Regular row
        SeqexecStyles.stepRow |+| stepRowStyle(s)
    }).htmlClass

  /**
    * Height depending if we use offsets
    */
  def baseHeight(p: Props): Int =
    if (p.showOffsets) {
      HeightWithOffsets
    } else {
      SeqexecStyles.rowHeight
    }

  /**
    * Calculates the row height depending on conditions
    */
  def rowHeight(b: Backend)(i: Int): Int =
    b.props.rowGetter(i) match {
      case StepRow(StandardStep(_, _, s, true, _, _, _, _))
          if s === StepState.Running =>
        // Row running with a breakpoint set
        SeqexecStyles.runningRowHeight + BreakpointLineHeight
      case StepRow(StandardStep(i, _, _, _, _, _, _, _))
          if b.state.selected.exists(_ === i) && b.props.canControlSubsystems(
            i) =>
        // Selected
        SeqexecStyles.runningRowHeight
      case StepRow(s: Step) if s.status === StepState.Running =>
        // Row running
        SeqexecStyles.runningRowHeight
      case StepRow(StandardStep(_, _, _, true, _, _, _, _)) =>
        // Row with a breakpoint set
        baseHeight(b.props) + BreakpointLineHeight
      case _ =>
        // default row
        baseHeight(b.props)
    }

  private val PhoneCut      = 412
  private val LargePhoneCut = 767

  val idxColumn: Table.ColumnArg =
    Column(
      Column.propsNoFlex(ColWidths.IdxWidth,
                         "idx",
                         label        = "Step",
                         className    = SeqexecStyles.paddedStepRow.htmlClass,
                         cellRenderer = stepIdRenderer))

  def stateColumn(b: Backend, controlWidth: Double): Option[Table.ColumnArg] =
    b.props.steps.map(
      i =>
        Column(
          Column
            .propsNoFlex(controlWidth,
                         "state",
                         label        = "Execution Progress",
                         className    = SeqexecStyles.paddedStepRow.htmlClass,
                         cellRenderer = stepProgressRenderer(i, b))
      ))

  def iconColumn(b: Backend): Option[Table.ColumnArg] =
    b.props.steps.map(
      i =>
        Column(
          Column.propsNoFlex(
            ColWidths.ControlWidth,
            "ctl",
            label = "Icon",
            cellRenderer = stepControlRenderer(i,
                                               b,
                                               rowBreakpointHoverOnCB(b),
                                               rowBreakpointHoverOffCB(b),
                                               recomputeRowHeightsCB),
            className      = SeqexecStyles.controlCellRow.htmlClass,
            headerRenderer = controlHeaderRenderer,
            headerClassName =
              (SeqexecStyles.centeredCell |+| SeqexecStyles.tableHeaderIcons).htmlClass
          )))

  def offsetColumn(p:             Props,
                   offsetVisible: Boolean): (Option[Table.ColumnArg], Double) =
    p.offsetsDisplay match {
      case OffsetsDisplay.DisplayOffsets(x) if p.showOffsets =>
        val width = ColWidths.OffsetWidthBase + x
        (Column(
           Column
             .propsNoFlex(
               width,
               "offset",
               label        = "Offset",
               cellRenderer = stepStatusRenderer(p.offsetsDisplay))).some
           .filter(_ => offsetVisible),
         width)
      case _ => (None, 0)
    }

  def disperserColumn(p:                Props,
                      disperserVisible: Boolean): Option[Table.ColumnArg] =
    p.steps
      .map(s =>
        Column(
          Column.propsNoFlex(
            ColWidths.DisperserWidth,
            "disperser",
            label = "Disperser",
            className = SeqexecStyles.centeredCell.htmlClass,
            cellRenderer = stepDisperserRenderer(s.instrument)
          )))
      .filter(_ => p.showDisperser && disperserVisible)

  def exposureColumn(p:               Props,
                     exposureVisible: Boolean): Option[Table.ColumnArg] =
    p.steps
      .map(i =>
        Column(
          Column.propsNoFlex(
            ColWidths.ExposureWidth,
            "exposure",
            label = "Exposure",
            className = SeqexecStyles.centeredCell.htmlClass,
            cellRenderer = stepExposureRenderer(i.instrument)
          )))
      .filter(_ => exposureVisible)

  def fpuColumn(p: Props, fpuVisible: Boolean): Option[Table.ColumnArg] =
    p.steps
      .map( i =>
        Column(
          Column.propsNoFlex(
            ColWidths.FPUWidth,
            "fpu",
            label = "FPU",
            className = SeqexecStyles.centeredCell.htmlClass,
            cellRenderer = stepFPURenderer(i.instrument))))
      .filter(_ => p.showFPU && fpuVisible)

  def observingModeColumn(p: Props): Option[Table.ColumnArg] =
    p.steps
      .map( _ =>
        Column(
          Column.propsNoFlex(
            ColWidths.ObservingModeWidth,
            "obsMode",
            label = "Observing Mode",
            className = SeqexecStyles.centeredCell.htmlClass,
            cellRenderer = stepObsModeRenderer
          )))
      .filter(_ => p.showObservingMode)

  def filterColumn(p: Props, filterVisible: Boolean): Option[Table.ColumnArg] =
    p.steps
      .map( i =>
          Column(
            Column.propsNoFlex(
              ColWidths.FilterWidth,
              "filter",
              label        = "Filter",
              className    = SeqexecStyles.centeredCell.htmlClass,
              cellRenderer = stepFilterRenderer(i.instrument)
            )))
      .filter(_ => p.showFilter && filterVisible)

  def typeColumn(p: Props, objectSize: SSize): Option[Table.ColumnArg] =
    p.steps.map(
      _ =>
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
            label          = "",
            cellRenderer   = settingsControlRenderer(p, i),
            className      = SeqexecStyles.settingsCellRow.htmlClass,
            headerRenderer = settingsHeaderRenderer,
            headerClassName =
              (SeqexecStyles.centeredCell |+| SeqexecStyles.tableHeaderIcons).htmlClass
          )))

  // Columns for the table
  private def columns(b: Backend, s: Size): List[Table.ColumnArg] = {
    val p = b.props
    val (offsetVisible, exposureVisible, disperserVisible, fpuVisible, filterVisible, objectSize) =
      s.width match {
        case w if w < PhoneCut =>
          (false, false, false, false, false, SSize.Tiny)
        case w if w < LargePhoneCut =>
          (false, true, false, false, false, SSize.Small)
        case _ =>
          (b.props.showOffsets, true, true, true, true, SSize.Small)
      }

    val (offsetCol, offsetWidth)        = offsetColumn(p, offsetVisible)
    val disperserCol                    = disperserColumn(p, disperserVisible)
    val observingModeCol                = observingModeColumn(p)
    val exposureCol                     = exposureColumn(p, exposureVisible)
    val fpuCol: Option[Table.ColumnArg] = fpuColumn(p, fpuVisible)
    val iconCol                         = iconColumn(b)
    val filterCol                       = filterColumn(p, filterVisible)
    val typeCol                         = typeColumn(p, objectSize)
    val settingsCol                     = settingsColumn(p)

    // Let's precisely calculate the width of the control column
    val colsWidth =
      ColWidths.ControlWidth +
        ColWidths.IdxWidth +
        offsetCol.fold(0.0)(_ => offsetWidth) +
        exposureCol.fold(0.0)(_ => ColWidths.ExposureWidth) +
        disperserCol.fold(0.0)(_ => ColWidths.DisperserWidth) +
        filterCol.fold(0.0)(_ => ColWidths.FilterWidth) +
        fpuCol.fold(0.0)(_ => ColWidths.FPUWidth) +
        observingModeCol.fold(0.0)(_ => ColWidths.ObservingModeWidth) +
        ColWidths.ObjectTypeWidth +
        ColWidths.SettingsWidth
    val controlWidth = s.width - colsWidth
    val stateCol     = stateColumn(b, controlWidth)

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

  def updateScrollPosition(b: Backend, pos: JsNumber): Callback = {
    val s = (State.userModified.set(IsModified) >>>
      State.scrollPosition.set(pos))
    b.modState(s) *>
      b.props.obsId
        .map(id =>
          SeqexecCircuit.dispatchCB(
            UpdateStepTableState(id, s(b.state).tableState)))
        .getOrEmpty *>
      Callback.empty
  }

  def startScrollTop(state: State): js.UndefOr[JsNumber] =
    if (state.tableState.userModified === IsModified) {
      state.tableState.scrollPosition
    } else {
      js.undefined
    }

  def startScrollToIndex(b: Backend): Int =
    if (b.state.tableState.userModified === IsModified) {
      -1
    } else {
      b.props.nextStepToRun
    }

  // Single click puts the row as selected
  def singleClick(b: Backend)(i: Int): Callback =
    b.props.obsId.map { id =>
      (SeqexecCircuit
        .dispatchCB(UpdateSelectedStep(id, i)) *>
        b.modState(State.selected.set(Some(i))) *>
        recomputeRowHeightsCB(min(b.state.selected.getOrElse(i), i)))
        .when(b.props
          .canControlSubsystems(i) && !b.props.tabOperations.resourceInFlight) *>
        Callback.empty
    }.getOrEmpty

  def stepsTableProps(b: Backend)(size: Size): Table.Props =
    Table.props(
      disableHeader = false,
      noRowsRenderer = () =>
        <.div(
          ^.cls := "ui center aligned segment noRows",
          ^.height := size.height.px,
          "No Steps"
      ),
      overscanRowCount = SeqexecStyles.overscanRowCount,
      height           = size.height.toInt,
      rowCount         = b.props.rowCount,
      rowHeight        = rowHeight(b) _,
      rowClassName     = rowClassName(b) _,
      width            = size.width.toInt,
      rowGetter        = b.props.rowGetter _,
      scrollToIndex    = startScrollToIndex(b),
      scrollTop        = startScrollTop(b.state),
      onRowClick       = singleClick(b),
      onScroll = (a, _, pos) =>
        updateScrollPosition(b, pos).when(a.toDouble > 0) *> Callback.empty,
      scrollToAlignment = ScrollToAlignment.Center,
      headerClassName   = SeqexecStyles.tableHeader.htmlClass,
      headerHeight      = SeqexecStyles.headerHeight,
      rowRenderer       = stopsRowRenderer
    )

  // We want clicks to be processed only if the click is not on the first row with the breakpoint/skip controls
  private def allowedClick(
    index: Int,
    onRowClick: Option[OnRowClick])(e: ReactMouseEvent): Callback =
    onRowClick
      .filter(_ => e.clientX > ColWidths.ControlWidth)
      .map(h => h(index))
      .getOrEmpty

  private def stopsRowRenderer =
    (className:        String,
     columns:          Array[VdomNode],
     index:            Int,
     _:                Boolean,
     key:              String,
     _:                StepRow,
     onRowClick:       Option[OnRowClick],
     onRowDoubleClick: Option[OnRowClick],
     _:                Option[OnRowClick],
     _:                Option[OnRowClick],
     _:                Option[OnRowClick],
     style:            Style) => {
      <.div(
        ^.cls := className,
        ^.key := key,
        ^.role := "row",
        ^.style := Style.toJsObject(style),
        ^.onClick ==> allowedClick(index, onRowClick),
        ^.onDoubleClick -->? onRowDoubleClick.map(h => h(index)),
        columns.toTagMod
      ): VdomElement
    }

  // Create a ref
  private val ref = Ref.toJsComponent(Table.component)

  private def recomputeRowHeightsCB(index: Int): Callback =
    ref.get.flatMapCB(_.raw.recomputeRowsHeightsCB(index))

  def rowBreakpointHoverOnCB(b: Backend)(index: Int): Callback =
    (if (b.props.rowGetter(index).step.breakpoint)
       b.modState(State.breakpointHover.set(None))
     else b.modState(State.breakpointHover.set(index.some))) *>
      recomputeRowHeightsCB(index)

  def rowBreakpointHoverOffCB(b: Backend)(index: Int): Callback =
    b.modState(State.breakpointHover.set(None)) *> recomputeRowHeightsCB(index)

  def receive(cur: Props, next: Props, s: State): Callback = {
    // Recalculate the heights if needed
    val stepsPairs = next.stepsList.zip(cur.stepsList)
    val differentStepsStates: List[StepId] = stepsPairs.collect {
      // if step status changes recalculate
      case (cur, prev) if cur.status =!= prev.status =>
        cur.id
      // if breakpoint state changes recalculate
      case (cur, prev) if cur.breakpoint =!= prev.breakpoint =>
        cur.id
    }
    val selected: Option[StepId] =
      (cur.selectedStep, next.selectedStep)
        .mapN { (c, n) =>
          min(c, n)
        }
        .filter(_ => s.selected =!= next.selectedStep)
    val running: Option[StepId] =
      if (cur.tabOperations.resourceRunRequested =!= next.tabOperations.resourceRunRequested) {
        next.selectedStep
      } else {
        none
      }
    (running.toList ::: selected.toList ::: differentStepsStates).minimumOption.map {
      recomputeRowHeightsCB
    }.getOrEmpty
  }

  // Wire it up from VDOM
  def render(b: Backend): VdomElement = {
    val p                 = b.props
    val settingsDisplayed = p.steps.forall(_.stepConfigDisplayed.isDefined)
    val hasControls       = (p.status.isLogged && !p.isPreview) || settingsDisplayed
    val noControls        = (p.isPreview || !p.status.isLogged) && !settingsDisplayed
    <.div(
      SeqexecStyles.stepsListPanePreview.when(noControls),
      SeqexecStyles.stepsListPaneWithControls.when(hasControls),
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
                ref.component(stepsTableProps(b)(s))(
                  columns(b, s).map(_.vdomElement): _*)))
          }
          .vdomElement
      }
    )
  }

  private val component = ScalaComponent
    .builder[Props]("StepsTable")
    .initialStateFromProps(_.startState)
    .render(render)
    .configure(Reusability.shouldComponentUpdate)
    .componentWillReceiveProps(x =>
      receive(x.currentProps, x.nextProps, x.state))
    .build

  def apply(p: Props): Unmounted[Props, State, Unit] = component(p)
}
