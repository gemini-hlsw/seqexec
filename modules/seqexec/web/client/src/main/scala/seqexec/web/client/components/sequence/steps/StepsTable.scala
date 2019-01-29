// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import japgolly.scalajs.react.component.builder.Lifecycle.ComponentWillReceiveProps
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.raw.JsNumber
import monocle.Lens
import monocle.macros.GenLens

import scala.scalajs.js
import scala.math.min
import react.common._
import react.common.syntax._
import seqexec.model.enum.Instrument
import seqexec.model.enum.StepType
import seqexec.model.StepState
import seqexec.model.Step
import seqexec.model.StepId
import seqexec.model.StandardStep
import seqexec.model.SequenceState
import seqexec.model.RunningStep
import seqexec.web.client.model.lenses._
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.model.TabOperations
import seqexec.web.client.model.Pages.SeqexecPages
import seqexec.web.client.model.ModelOps._
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.circuit.StepsTableAndStatusFocus
import seqexec.web.client.circuit.StepsTableFocus
import seqexec.web.client.actions.{ClearAllResouceOptions, UpdateSelectedStep, UpdateStepTableState}
import seqexec.web.client.actions.FlipBreakpointStep
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.components.TableContainer
import seqexec.web.client.components.sequence.steps.OffsetFns._
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.semanticui.{Size => SSize}
import seqexec.web.client.reusability._
import react.virtualized._
import web.client.style._
import web.client.table._

trait Columns {
  val ControlWidth: Double       = 40
  val StepWidth: Double          = 50
  val StateWidth: Double         = 200
  val OffsetWidthBase: Double    = 75
  val ExposureWidth: Double      = 75
  val ExposureMinWidth: Double   = 78.95 + SeqexecStyles.TableBorderWidth
  val DisperserWidth: Double     = 100
  val DisperserMinWidth: Double  = 100 + SeqexecStyles.TableBorderWidth
  val ObservingModeWidth: Double = 180
  val FilterWidth: Double        = 180
  val FPUWidth: Double           = 100
  val DeckerWidth: Double        = 110
  val ImagingMirrorWidth: Double = 180
  val CameraWidth: Double        = 180
  val ReadModeWidth: Double      = 230
  val ObjectTypeWidth: Double    = 75
  val SettingsWidth: Double      = 34

  private val MIDDLE_BUTTON = 1 // As defined by React.js

  sealed trait TableColumn extends Product with Serializable
  case object ControlColumn extends TableColumn
  case object StepColumn extends TableColumn
  case object ExecutionColumn extends TableColumn
  case object OffsetColumn extends TableColumn
  case object ObservingModeColumn extends TableColumn
  case object ExposureColumn extends TableColumn
  case object DisperserColumn extends TableColumn
  case object FilterColumn extends TableColumn
  case object FPUColumn extends TableColumn
  case object CameraColumn extends TableColumn
  case object ObjectTypeColumn extends TableColumn
  case object SettingsColumn extends TableColumn

  val columnsDefaultWidth: Map[TableColumn, Double] = Map(
    ControlColumn -> ControlWidth,
    StepColumn -> StepWidth,
    ExecutionColumn -> StateWidth,
    OffsetColumn -> OffsetWidthBase,
    ObservingModeColumn -> ObservingModeWidth,
    ExposureColumn -> ExposureWidth,
    DisperserColumn -> DisperserWidth,
    FilterColumn -> FilterWidth,
    FPUColumn -> FPUWidth,
    CameraColumn -> CameraWidth,
    ObjectTypeColumn -> ObjectTypeWidth,
    SettingsColumn -> SettingsWidth,
  )

  object TableColumn {
    implicit val equal: Eq[TableColumn] = Eq.fromUniversalEquals
  }

  val ControlColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    ControlColumn,
    name    = "control",
    label   = "",
    visible = true,
    FixedColumnWidth.unsafeFromDouble(ControlWidth))

  val StepMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    StepColumn,
    name    = "idx",
    label   = "Step",
    visible = true,
    FixedColumnWidth.unsafeFromDouble(StepWidth))

  val ExecutionMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    ExecutionColumn,
    name    = "state",
    label   = "Execution Progress",
    visible = true,
    VariableColumnWidth.unsafeFromDouble(0.1, StateWidth))

  val OffsetMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    OffsetColumn,
    name    = "state",
    label   = "Execution Progress",
    visible = true,
    VariableColumnWidth.unsafeFromDouble(0.1, OffsetWidthBase))

  val ObservingModeMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    ObservingModeColumn,
    name    = "obsMode",
    label   = "Observing Mode",
    visible = true,
    VariableColumnWidth.unsafeFromDouble(0.1, ObservingModeWidth))

  val ExposureMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    ExposureColumn,
    name    = "exposure",
    label   = "Exposure",
    visible = true,
    VariableColumnWidth.unsafeFromDouble(0.1, ExposureMinWidth))

  val DisperserMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    DisperserColumn,
    name    = "disperser",
    label   = "Disperser",
    visible = true,
    VariableColumnWidth.unsafeFromDouble(0.1, DisperserMinWidth))

  val FilterMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    FilterColumn,
    name    = "filter",
    label   = "Filter",
    visible = true,
    VariableColumnWidth.unsafeFromDouble(0.1, FilterWidth))

  val FPUMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    FPUColumn,
    name    = "camera",
    label   = "FPU",
    visible = true,
    VariableColumnWidth.unsafeFromDouble(0.1, FPUWidth))

  val CameraMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    CameraColumn,
    name    = "camera",
    label   = "Camera",
    visible = true,
    VariableColumnWidth.unsafeFromDouble(0.1, CameraWidth))

  val ObjectTypeMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    ObjectTypeColumn,
    name    = "type",
    label   = "Type",
    visible = true,
    FixedColumnWidth.unsafeFromDouble(ObjectTypeWidth))

  val SettingsMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    SettingsColumn,
    name    = "set",
    label   = "",
    visible = true,
    FixedColumnWidth.unsafeFromDouble(SettingsWidth))

  val all: NonEmptyList[ColumnMeta[TableColumn]] =
    NonEmptyList.of(
      ControlColumnMeta,
      StepMeta,
      ExecutionMeta,
      OffsetMeta,
      ObservingModeMeta,
      ExposureMeta,
      DisperserMeta,
      FilterMeta,
      FPUMeta,
      CameraMeta,
      ObjectTypeMeta,
      SettingsMeta
    )

}

/**
  * Container for a table with the steps
  */
object StepsTable extends Columns {
  type Backend = RenderScope[Props, State, Unit]
  type ReceiveProps = ComponentWillReceiveProps[Props, State, Unit]

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

  final case class Props(router:     RouterCtl[SeqexecPages],
                         canOperate: Boolean,
                         stepsTable: StepsTableAndStatusFocus) {
    val status: ClientStatus                        = stepsTable.status
    val steps: Option[StepsTableFocus]              = stepsTable.stepsTable
    val sequenceState: Option[SequenceState]        = steps.map(_.state)
    val runningStep: Option[RunningStep]            = steps.flatMap(_.runningStep)
    val obsId: Option[Observation.Id]               = steps.map(_.id)
    val tableState: Option[TableState[TableColumn]] = steps.map(_.tableState)
    val stepsList: List[Step]                       = steps.foldMap(_.steps)
    val selectedStep: Option[StepId]                = steps.flatMap(_.selectedStep)
    val rowCount: Int                               = stepsList.length
    val nextStepToRun: Int                          = steps.foldMap(_.nextStepToRun).getOrElse(0)
    val tabOperations: TabOperations =
      steps.map(_.tabOperations).getOrElse(TabOperations.Default)
    val showDisperser: Boolean     = showProp(InstrumentProperties.Disperser)
    val showExposure: Boolean      = showProp(InstrumentProperties.Exposure)
    val showFilter: Boolean        = showProp(InstrumentProperties.Filter)
    val showFPU: Boolean           = showProp(InstrumentProperties.FPU)
    val showCamera: Boolean        = showProp(InstrumentProperties.Camera)
    val showDecker: Boolean        = showProp(InstrumentProperties.Decker)
    val showImagingMirror: Boolean = showProp(InstrumentProperties.ImagingMirror)
    val showReadMode: Boolean      = showProp(InstrumentProperties.ReadMode)
    val isPreview: Boolean         = steps.map(_.isPreview).getOrElse(false)
    val hasControls: Boolean       = canOperate && !isPreview
    val canSetBreakpoint: Boolean  = canOperate && !isPreview
    val showObservingMode: Boolean = showProp(InstrumentProperties.ObservingMode)

    def stepSelectionAllowed(sid: StepId): Boolean =
      canControlSubsystems(sid) && !tabOperations.resourceInFlight && !sequenceState.exists(_.isRunning)

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
        .map(
          s =>
            (State.tableState.set(s) >>> State.selected.set(selectedStep))(
              State.InitialState))
        .getOrElse(State.InitialState)
        .visibleCols(this)
  }

  final case class State(tableState:      TableState[TableColumn],
                         breakpointHover: Option[Int],
                         selected:        Option[StepId]) {

    // Hide some columns depending on width
    def hideOnWidth(s: Size): State = {
      // s.width match {
      //   case w if w < PhoneCut =>
      //     State.columns.modify(_.map {
      //       case c @ ColumnMeta(ObsNameColumn, _, _, _, _) =>
      //         c.copy(visible = false)
      //       case c @ ColumnMeta(TargetNameColumn, _, _, _, _) =>
      //         c.copy(visible = false)
      //       case c =>
      //         c
      //     })(this)
      //   case w if w < LargePhoneCut =>
      //     State.columns.modify(_.map {
      //       case c @ ColumnMeta(TargetNameColumn, _, _, _, _) =>
      //         c.copy(visible = false)
      //       case c =>
      //         c
      //     })(this)
      //   case _ =>
      //     this
      // println(s)
      this
    }

    def shownForInstrument(p: Props): List[ColumnMeta[TableColumn]] =
      all.filter {
        case DisperserMeta     => p.showDisperser
        case OffsetMeta        => p.showOffsets
        case ObservingModeMeta => p.showObservingMode
        case ExposureMeta      => p.showExposure
        case FilterMeta        => p.showFilter
        case FPUMeta           => p.showFPU
        case CameraMeta        => p.showCamera
        case _                 => true
      }

    def visibleCols(p: Props): State =
      State.columns.set(NonEmptyList.fromListUnsafe(shownForInstrument(p)))(
        this)

    // def visibleColumnsSizes(p: Props, s: Size): List[(TableColumn, Double, Boolean)] = {
    //   val visibleCols = shownForInstrument(p)
    //   val fixedVisibleCols = 2
    //   for {
    //     (c, i) <- visibleCols.toList.zipWithIndex // TODO hide on width
    //     if c.visible
    //   } yield
    //     (c.column,
    //      tableState.widthOf(c.column, s),
    //      i === tableState.columns.filter(_.visible).length - fixedVisibleCols)
    // }
    // calculate the relative widths of each column based on content only
    // this should be renormalized against the actual tabel width
    def withWidths(steps: List[Step]): State = {
      // if (tableState.userModified === IsModified) {
      //   this
      // } else {
      //   val optimalSizes = steps.foldLeft(columnsDefaultWidth) {
      //     case (currWidths,
      //           SequenceInSessionQueue(id, st, i, _, _, n, _, t, r, _, _)) =>
      //       val idWidth = max(
      //         currWidths.getOrElse(ObsIdColumn, ObsIdMinWidth),
      //         tableTextWidth(id.format)) + SeqexecStyles.TableRightPadding
      //       val statusWidth =
      //         max(currWidths.getOrElse(StateColumn, StateMinWidth),
      //             tableTextWidth(statusText(st, r)))
      //       val instrumentWidth =
      //         max(currWidths.getOrElse(InstrumentColumn, InstrumentMinWidth),
      //             tableTextWidth(i.show))
      //       val targetNameWidth =
      //         max(currWidths.getOrElse(TargetNameColumn, TargetMinWidth),
      //             tableTextWidth(t.getOrElse("")))
      //       val obsNameWidth =
      //         max(currWidths.getOrElse(ObsNameColumn, ObsNameMinWidth),
      //             tableTextWidth(n))
      //
      //       currWidths +
      //         (ObsIdColumn -> idWidth) +
      //         (StateColumn -> statusWidth) +
      //         (InstrumentColumn -> instrumentWidth) +
      //         (ObsNameColumn -> obsNameWidth) +
      //         (TargetNameColumn -> targetNameWidth)
      //   }
      //   // Width as it would be adding all the visible columns
      //   val width = optimalSizes
      //     .filter {
      //       case (c, _) =>
      //         tableState.columns.find(_.column === c).forall(_.visible)
      //     }
      //     .values
      //     .sum + ClassColumnWidth + (if (loggedIn) AddQueueColumnWidth else 0)
      //   // Normalize based on visibility
      //   State.columns.modify(_.map {
      //     case c @ ColumnMeta(t, _, _, true, VariableColumnWidth(_, m)) =>
      //       VariableColumnWidth
      //         .fromDouble(optimalSizes.getOrElse(t, m).toDouble / width, m)
      //         .fold(c)(w => c.copy(width = w))
      //     case c =>
      //       c
      //   })(this)
      // }
      // println(steps.length)
      this
    }
  }

  object State {

    val tableState: Lens[State, TableState[TableColumn]] =
      GenLens[State](_.tableState)

    val breakpointHover: Lens[State, Option[Int]] =
      GenLens[State](_.breakpointHover)

    val selected: Lens[State, Option[StepId]] =
      GenLens[State](_.selected)

    val columns: Lens[State, NonEmptyList[ColumnMeta[TableColumn]]] =
      tableState ^|-> TableState.columns[TableColumn]

    val scrollPosition: Lens[State, JsNumber] =
      tableState ^|-> TableState.scrollPosition[TableColumn]

    val userModified: Lens[State, UserModified] =
      tableState ^|-> TableState.userModified[TableColumn]

    val InitialTableState: TableState[TableColumn] =
      TableState(NotModified, 0, all)

    val InitialState: State = State(InitialTableState, None, None)
  }

  implicit val propsReuse: Reusability[Props] =
    Reusability.by(x => (x.canOperate, x.selectedStep, x.stepsTable))
  implicit val tcReuse: Reusability[TableColumn] = Reusability.byRef
  implicit val stateReuse: Reusability[State] =
    Reusability.by(x => (x.tableState, x.breakpointHover, x.selected))

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
                               b.props.tabOperations))

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

  def cameraRenderer(
    i: Instrument
  ): CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) => CameraCell(CameraCell.Props(row.step, i))

  def deckerRenderer: CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) => DeckerCell(DeckerCell.Props(row.step))

  def imagingMirrorRenderer: CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      ImagingMirrorCell(ImagingMirrorCell.Props(row.step))

  def readModeRenderer: CellRenderer[js.Object, js.Object, StepRow] =
    (_, _, _, row: StepRow, _) =>
      ReadModeCell(ReadModeCell.Props(row.step))

  private def stepRowStyle(step: Step): GStyle = step match {
    case s if s.hasError                       => SeqexecStyles.rowError
    case s if s.status === StepState.Running   => SeqexecStyles.rowWarning
    case s if s.status === StepState.Paused    => SeqexecStyles.rowWarning
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

  // private val PhoneCut      = 412
  // private val LargePhoneCut = 767

  def columnClassName(c: TableColumn): Option[GStyle] =
    c match {
      case ControlColumn                => SeqexecStyles.controlCellRow.some
      case StepColumn | ExecutionColumn => SeqexecStyles.paddedStepRow.some
      case ObservingModeColumn | ExposureColumn | DisperserColumn |
          FilterColumn | FPUColumn | CameraColumn | ObjectTypeColumn =>
        SeqexecStyles.centeredCell.some
      case SettingsColumn => SeqexecStyles.settingsCellRow.some
      case _              => none
    }

  def headerClassName(c: TableColumn): Option[GStyle] =
    c match {
      case ControlColumn =>
        (SeqexecStyles.centeredCell |+| SeqexecStyles.tableHeaderIcons).some
      case SettingsColumn =>
        (SeqexecStyles.centeredCell |+| SeqexecStyles.tableHeaderIcons).some
      case _ => none
    }

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

  private def fixedHeaderRenderer(c: TableColumn): HeaderRenderer[js.Object] =
    c match {
      case ControlColumn  => controlHeaderRenderer
      case SettingsColumn => settingsHeaderRenderer
      case _              => defaultHeaderRendererS
    }

  private def columnCellRenderer(
    b: Backend,
    c: TableColumn): CellRenderer[js.Object, js.Object, StepRow] = {
    val optR = c match {
      case ControlColumn =>
        b.props.steps.map(
          stepControlRenderer(_,
                              b,
                              rowBreakpointHoverOnCB(b),
                              rowBreakpointHoverOffCB(b),
                              recomputeRowHeightsCB))
      case StepColumn          => stepIdRenderer.some
      case ExecutionColumn     => b.props.steps.map(stepProgressRenderer(_, b))
      case OffsetColumn        => stepStatusRenderer(b.props.offsetsDisplay).some
      case ObservingModeColumn => stepObsModeRenderer.some
      case ExposureColumn =>
        b.props.steps.map(p => stepExposureRenderer(p.instrument))
      case DisperserColumn =>
        b.props.steps.map(p => stepDisperserRenderer(p.instrument))
      case FilterColumn =>
        b.props.steps.map(p => stepFilterRenderer(p.instrument))
      case FPUColumn        => b.props.steps.map(p => stepFPURenderer(p.instrument))
      case CameraColumn     => b.props.steps.map(p => cameraRenderer(p.instrument))
      case ObjectTypeColumn => stepObjectTypeRenderer(SSize.Small).some
      case SettingsColumn =>
        b.props.steps.map(p => settingsControlRenderer(b.props, p))
      case _ => none
    }
    optR.getOrElse(defaultCellRendererS)
  }

  // Columns for the table
  private def colBuilder(
    b:    Backend,
    size: Size): ColumnRenderArgs[TableColumn] => Table.ColumnArg = tb => {
    def updateState(s: TableState[TableColumn]): Callback =
      b.modState(State.tableState.set(s)) // >> SeqexecCircuit.dispatchCB(UpdateStepsConfigTableState(s))

    tb match {
      case ColumnRenderArgs(ColumnMeta(c, name, label, _, _), _, width, true) =>
        Column(
          Column.propsNoFlex(
            width   = width,
            dataKey = name,
            label   = label,
            headerRenderer = resizableHeaderRenderer(
              b.state.tableState.resizeRowB(c, size, updateState)),
            headerClassName = headerClassName(c).foldMap(_.htmlClass),
            cellRenderer    = columnCellRenderer(b, c),
            className       = columnClassName(c).foldMap(_.htmlClass)
          ))
      case ColumnRenderArgs(ColumnMeta(c, name, label, _, _),
                            _,
                            width,
                            false) =>
        Column(
          Column.propsNoFlex(
            width           = width,
            dataKey         = name,
            label           = label,
            headerRenderer  = fixedHeaderRenderer(c),
            headerClassName = headerClassName(c).foldMap(_.htmlClass),
            cellRenderer    = columnCellRenderer(b, c),
            className       = columnClassName(c).foldMap(_.htmlClass)
          ))
    }
  }
  // scalastyle:on

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
       SeqexecCircuit
        .dispatchCB(ClearAllResouceOptions(id)) *>
        b.modState(State.selected.set(Some(i))) *>
        recomputeRowHeightsCB(min(b.state.selected.getOrElse(i), i)))
        .when(b.props.stepSelectionAllowed(i)) *>
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
      rowRenderer       = stopsRowRenderer(b.props)
    )

  // We want clicks to be processed only if the click is not on the first row with the breakpoint/skip controls
  private def allowedClick(p: Props, index: Int, onRowClick: Option[OnRowClick])(
    e:                            ReactMouseEvent): Callback =
      // If alt is pressed or middle button flip the breakpoint
      if (e.altKey || e.button === MIDDLE_BUTTON) {
        Callback.when(p.canSetBreakpoint)(
          (p.obsId, p.stepsList.find(_.id === index + 1)).mapN((oid, step) =>
            SeqexecCircuit.dispatchCB(FlipBreakpointStep(oid, step))).getOrEmpty
        )
      } else {
        onRowClick
          .filter(_ => e.clientX > ControlWidth)
          .map(h => h(index))
          .getOrEmpty
        }

  private def stopsRowRenderer(p: Props) =
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
        ^.onClick ==> allowedClick(p, index, onRowClick),
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

  private def updateStep(b:     ReceiveProps,
                         p:     Props,
                         obsId: Observation.Id,
                         i:     StepId): Callback =
    (SeqexecCircuit.dispatchCB(UpdateSelectedStep(obsId, i)) *>
      b.modState(State.selected.set(i.some)))
      .when(p.canControlSubsystems(i)) *> Callback.empty

  // We need to update the state if the props change
  def receiveNewProps(b: ReceiveProps): Callback = {
    val (cur: Props, next: Props) = (b.currentProps, b.nextProps)
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

    // Update the selected step as the run proceeds
    val selectStep = (next.obsId, cur.runningStep, next.runningStep) match {
      case (Some(obsId), Some(RunningStep(i, _)), None) =>
        updateStep(b, next, obsId, i)
      case (Some(obsId), Some(RunningStep(i, _)), Some(RunningStep(j, _)))
          if i =!= j =>
        updateStep(b, next, obsId, j)
      case (Some(obsId), _, Some(RunningStep(j, _))) if cur.sequenceState =!= next.sequenceState && next.sequenceState.exists(_.isRunning) =>
        // When we start running select the running step
        updateStep(b, next, obsId, j)
      case _ =>
        Callback.empty
    }

    // If the selected step changes recompute height
    val selected: Option[StepId] =
      (cur.selectedStep, next.selectedStep).mapN { (c, n) =>
          min(c, n)
        }
        .filter(_ => b.state.selected =!= next.selectedStep)

    // The selected step may have changed externally
    val selectedStepChange: Callback =
      (cur.selectedStep, next.selectedStep).mapN { (c, n) =>
          b.modState(State.selected.set(n.some)).when(c =!= n) *> Callback.empty
        }.getOrEmpty

    // If the step is running recalculate height
    val running: Option[StepId] =
      if (cur.tabOperations.resourceRunRequested =!= next.tabOperations.resourceRunRequested) {
        next.selectedStep
      } else {
        none
      }

    selectStep *> selectedStepChange *> (running.toList ::: selected.toList ::: differentStepsStates).minimumOption.map {
      recomputeRowHeightsCB
    }.getOrEmpty
  }

  // Wire it up from VDOM
  def render(b: Backend): VdomElement =
    TableContainer(
      TableContainer.Props(
        b.props.hasControls,
        size =>
          ref
            .component(stepsTableProps(b)(size))(
              b.state.tableState
                .columnBuilderB(size, colBuilder(b, size))
                .map(_.vdomElement): _*)
            .vdomElement
      ))

  private val component = ScalaComponent
    .builder[Props]("StepsTable")
    .initialStateFromProps(_.startState)
    .render(render)
    .configure(Reusability.shouldComponentUpdate)
    .componentWillReceiveProps(receiveNewProps)
    .build

  def apply(p: Props): Unmounted[Props, State, Unit] = component(p)
}
