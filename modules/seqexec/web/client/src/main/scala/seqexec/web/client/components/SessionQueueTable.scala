// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import cats.implicits._
import cats.data.NonEmptyList
import cats.Eq
import gem.Observation
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.raw.JsNumber
import mouse.all._
import monocle.Lens
import monocle.macros.GenLens
import react.virtualized._
import scala.math.max
import scala.scalajs.js
import seqexec.model.enum.Instrument
import seqexec.model.UserDetails
import seqexec.model.UnknownTargetName
import seqexec.model.Observer
import seqexec.model.SequenceState
import seqexec.model.CalibrationQueueId
import seqexec.web.client.circuit._
import seqexec.web.client.actions._
import seqexec.web.client.model.Pages._
import seqexec.web.client.model.ObsClass
import seqexec.web.client.model.RunningStep
import seqexec.web.client.model.SessionQueueFilter
import seqexec.web.client.model.ModelOps._
import seqexec.web.client.semanticui.elements.icon.Icon.IconAttention
import seqexec.web.client.semanticui.elements.icon.Icon.IconCheckmark
import seqexec.web.client.semanticui.elements.icon.Icon.IconCircleNotched
import seqexec.web.client.semanticui.elements.icon.Icon.IconRefresh
import seqexec.web.client.semanticui.elements.icon.Icon.IconSelectedRadio
import seqexec.web.client.semanticui.elements.icon.Icon.IconSun
import seqexec.web.client.semanticui.elements.icon.Icon.IconMoon
import seqexec.web.client.semanticui.elements.icon.Icon.IconCircleOutline
import seqexec.web.client.semanticui.elements.icon.Icon.IconCheckCircleOutline
import seqexec.web.client.semanticui.elements.icon.Icon.IconCalendarOutline
import seqexec.web.client.semanticui.elements.icon.Icon.IconClockOutline
import seqexec.web.client.semanticui.{ Size => SSize }
import seqexec.web.client.reusability._
import web.client.style._
import web.client.utils._
import web.client.table._

object SessionQueueTable {
  type Backend = RenderScope[Props, State, Unit]

  private val PhoneCut              = 400.0
  private val LargePhoneCut         = 570.0
  private val IconColumnWidth       = 25.0
  private val AddQueueColumnWidth   = 30.0
  private val ClassColumnWidth      = 26.0
  private val ObsIdColumnWidth      = 140.0
  private val ObsIdMinWidth         = 66.2167 + SeqexecStyles.TableBorderWidth
  private val StateColumnWidth      = 80.0
  private val StateMinWidth         = 53.3667 + SeqexecStyles.TableBorderWidth
  private val InstrumentColumnWidth = 80.0
  private val InstrumentMinWidth    = 90.4333 + SeqexecStyles.TableBorderWidth
  private val TargetNameColumnWidth = 140.0
  private val TargetMinWidth        = 60.0167 + SeqexecStyles.TableBorderWidth
  private val ObsNameColumnWidth    = 140.0
  private val ObsNameMinWidth       = 60.0167 + SeqexecStyles.TableBorderWidth
  private val SessionQueueColumnStyle: String =
    SeqexecStyles.queueTextColumn.htmlClass

  sealed trait TableColumn extends Product with Serializable
  case object IconColumn       extends TableColumn
  case object AddQueueColumn   extends TableColumn
  case object ClassColumn      extends TableColumn
  case object ObsIdColumn      extends TableColumn
  case object StateColumn      extends TableColumn
  case object InstrumentColumn extends TableColumn
  case object ObsNameColumn    extends TableColumn
  case object TargetNameColumn extends TableColumn

  object TableColumn {
    implicit val equal: Eq[TableColumn] = Eq.fromUniversalEquals
  }

  val IconColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    IconColumn,
    name    = "status",
    label   = "",
    visible = true,
    FixedColumnWidth.unsafeFromDouble(IconColumnWidth))

  val ClassColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    ClassColumn,
    name    = "class",
    label   = "",
    visible = true,
    FixedColumnWidth.unsafeFromDouble(ClassColumnWidth))

  val AddQueueColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    AddQueueColumn,
    name    = "",
    label   = "",
    visible = true,
    FixedColumnWidth.unsafeFromDouble(AddQueueColumnWidth))

  val ObsIdColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    ObsIdColumn,
    name    = "obsid",
    label   = "Obs. ID",
    visible = true,
    PercentageColumnWidth.unsafeFromDouble(1.0, ObsIdMinWidth))

  val StateColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    StateColumn,
    name    = "state",
    label   = "State",
    visible = true,
    PercentageColumnWidth.unsafeFromDouble(1.0, StateMinWidth))

  val InstrumentColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    InstrumentColumn,
    name    = "instrument",
    label   = "Instrument",
    visible = true,
    PercentageColumnWidth.unsafeFromDouble(1.0, InstrumentMinWidth))

  val TargetNameColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    TargetNameColumn,
    name    = "target",
    label   = "Target",
    visible = true,
    PercentageColumnWidth.unsafeFromDouble(1.0, TargetMinWidth))

  val ObsNameColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    ObsNameColumn,
    name    = "obsName",
    label   = "Obs. Name",
    visible = true,
    PercentageColumnWidth.unsafeFromDouble(1.0, ObsNameMinWidth))

  val all: NonEmptyList[ColumnMeta[TableColumn]] = NonEmptyList.of(
    IconColumnMeta,
    AddQueueColumnMeta,
    ClassColumnMeta,
    ObsIdColumnMeta,
    StateColumnMeta,
    InstrumentColumnMeta,
    TargetNameColumnMeta,
    ObsNameColumnMeta)

  val columnsDefaultWidth: Map[TableColumn, Double] = Map(
    IconColumn -> IconColumnWidth,
    AddQueueColumn -> AddQueueColumnWidth,
    ClassColumn -> ClassColumnWidth,
    ObsIdColumn -> ObsIdColumnWidth,
    StateColumn -> StateColumnWidth,
    InstrumentColumn -> InstrumentColumnWidth,
    TargetNameColumn -> TargetNameColumnWidth,
    ObsNameColumn -> ObsNameColumnWidth
  )

  final case class Props(ctl:       RouterCtl[SeqexecPages],
                         sequences: StatusAndLoadedSequencesFocus) {
    val sequencesList: List[SequenceInSessionQueue] =
      sequences.queueFilter.filter(sequences.sequences)

    val startState: TableState[TableColumn] = sequences.tableState

    def rowGetter(i: Int): SessionQueueRow =
      sequencesList
        .lift(i)
        .map { s =>
          SessionQueueRow(s.id,
                          s.status,
                          s.instrument,
                          s.targetName,
                          s.name,
                          s.obsClass,
                          s.active,
                          s.loaded,
                          s.nextStepToRun,
                          s.runningStep,
                          s.inDayCalQueue)
        }
        .getOrElse(SessionQueueRow.Empty)

    val rowCount: Int =
      sequencesList.size

    val canOperate: Boolean = sequences.status.canOperate

    val user: Option[UserDetails] = sequences.status.u
  }

  final case class State(tableState: TableState[TableColumn],
                         rowLoading: Option[Int],
                         loggedIn:   Boolean) {
    // Update the columns' visibility based on logged in state
    private def logIn: State =
      State.columns
        .modify(_.map(_.copy(visible = true)))
        .andThen(State.loggedIn.set(true))(this)

    // Update the columns' visibility based on logged off state
    private def logOff: State =
      State.loggedIn
        .set(false)
        .andThen(State.columns.modify(_.map {
          case c @ ColumnMeta(ObsNameColumn, _, _, _, _) =>
            c.copy(visible = false)
          case c @ ColumnMeta(AddQueueColumn, _, _, _, _) =>
            c.copy(visible = false)
          case c @ ColumnMeta(TargetNameColumn, _, _, _, _) =>
            c.copy(visible = false)
          case c =>
            c
        }))(this)

    // Change the columns visibility depending on the logged in state
    def loginState(canOperate: Boolean): State = {
      val loginChanged = canOperate =!= loggedIn
      State.userModified.modify(m =>
        UserModified.fromBool((m === IsModified) && !loginChanged))(
        canOperate.fold(logIn, logOff))
    }

    // Reset loading of rows
    def resetLoading(p: Props): State =
      if (rowLoading.exists(i => p.rowGetter(i).loaded)) {
        copy(rowLoading = None)
      } else {
        this
      }

    // calculate the relative widths of each column based on content only
    // this should be renormalized against the actual tabel width
    def withWidths(sequences: List[SequenceInSessionQueue]): State =
      if (tableState.userModified === IsModified) {
        this
      } else {
        val optimalSizes = sequences.foldLeft(columnsDefaultWidth) {
          case (currWidths,
                SequenceInSessionQueue(id, st, i, _, _, n, _, t, r, _, _)) =>
            val idWidth = max(
              currWidths.getOrElse(ObsIdColumn, ObsIdMinWidth),
              tableTextWidth(id.format)) + SeqexecStyles.TableRightPadding
            val statusWidth =
              max(currWidths.getOrElse(StateColumn, StateMinWidth),
                  tableTextWidth(statusText(st, r)))
            val instrumentWidth =
              max(currWidths.getOrElse(InstrumentColumn, InstrumentMinWidth),
                  tableTextWidth(i.show))
            val targetNameWidth =
              max(currWidths.getOrElse(TargetNameColumn, TargetMinWidth),
                  tableTextWidth(t.getOrElse("")))
            val obsNameWidth =
              max(currWidths.getOrElse(ObsNameColumn, ObsNameMinWidth),
                  tableTextWidth(n))

            currWidths +
              (ObsIdColumn -> idWidth) +
              (StateColumn -> statusWidth) +
              (InstrumentColumn -> instrumentWidth) +
              (ObsNameColumn -> obsNameWidth) +
              (TargetNameColumn -> targetNameWidth)
        }
        // Width as it would be adding all the visible columns
        val width = optimalSizes
          .filter {
            case (c, _) =>
              tableState.columns.find(_.column === c).forall(_.visible)
          }
          .values
          .sum + ClassColumnWidth + (if (loggedIn) AddQueueColumnWidth else 0)
        // Normalize based on visibility
        State.columns.modify(_.map {
          case c @ ColumnMeta(t, _, _, true, PercentageColumnWidth(_, m)) =>
            PercentageColumnWidth
              .fromDouble(optimalSizes.getOrElse(t, m).toDouble / width, m)
              .fold(c)(w => c.copy(width = w))
          case c =>
            c
        })(this)
      }

    // Returns a list of the visible columns with the suggested size
    def visibleColumnsSizes(s: Size): List[(TableColumn, Double, Boolean)] = {
      val fixedVisibleCols = if (loggedIn) 1 else 0
      for {
        (c, i) <- hideOnWidth(s).tableState.columns.toList.zipWithIndex
        if c.visible
      } yield
        (c.column,
         tableState.widthOf(c.column, s),
         i === tableState.columns.filter(_.visible).length - fixedVisibleCols)
    }

    // Hide some columns depending on width
    private def hideOnWidth(s: Size): State =
      s.width match {
        case w if w < PhoneCut =>
          State.columns.modify(_.map {
            case c @ ColumnMeta(ObsNameColumn, _, _, _, _) =>
              c.copy(visible = false)
            case c @ ColumnMeta(TargetNameColumn, _, _, _, _) =>
              c.copy(visible = false)
            case c =>
              c
          })(this)
        case w if w < LargePhoneCut =>
          State.columns.modify(_.map {
            case c @ ColumnMeta(TargetNameColumn, _, _, _, _) =>
              c.copy(visible = false)
            case c =>
              c
          })(this)
        case _ =>
          this
      }

    def applyOffset(column: TableColumn, delta: Double, s: Size): State =
      State.tableState.modify(_.applyOffset(column, delta, s))(this)
  }

  val InitialTableState: State =
    State(TableState(NotModified, 0, all), None, false)

  object State {
    // Lenses
    val loggedIn: Lens[State, Boolean] = GenLens[State](_.loggedIn)

    val tableState: Lens[State, TableState[TableColumn]] =
      GenLens[State](_.tableState)

    val columns: Lens[State, NonEmptyList[ColumnMeta[TableColumn]]] =
      tableState ^|-> TableState.columns[TableColumn]

    val userModified: Lens[State, UserModified] =
      tableState ^|-> TableState.userModified[TableColumn]

    val scrollPosition: Lens[State, JsNumber] =
      tableState ^|-> TableState.scrollPosition[TableColumn]
  }

  implicit val tcReuse: Reusability[TableColumn] = Reusability.byRef
  implicit val sqSeFocusReuse: Reusability[SequenceInSessionQueue] =
    Reusability.byEq
  implicit val qfReuse: Reusability[SessionQueueFilter] =
    Reusability.byEq
  implicit val stSeFocusReuse: Reusability[StatusAndLoadedSequencesFocus] =
    Reusability.by(x => (x.status, x.sequences, x.tableState, x.queueFilter))
  implicit val propsReuse: Reusability[Props] = Reusability.by(_.sequences)
  implicit val stateReuse: Reusability[State] = Reusability.derive[State]

  // ScalaJS defined trait
  // scalastyle:off
  trait SessionQueueRow extends js.Object {
    var obsId: Observation.Id
    var status: SequenceState
    var instrument: Instrument
    var targetName: Option[String]
    var name: String
    var obsClass: ObsClass
    var active: Boolean
    var loaded: Boolean
    var nextStepToRun: Option[Int]
    var runningStep: Option[RunningStep]
    var inDayCalQueue: Boolean
  }

  // scalastyle:on
  object SessionQueueRow {

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def apply(obsId:         Observation.Id,
              status:        SequenceState,
              instrument:    Instrument,
              targetName:    Option[String],
              name:          String,
              obsClass:      ObsClass,
              active:        Boolean,
              loaded:        Boolean,
              nextStepToRun: Option[Int],
              runningStep:   Option[RunningStep],
              inDayCalQueue: Boolean): SessionQueueRow = {
      val p = (new js.Object).asInstanceOf[SessionQueueRow]
      p.obsId         = obsId
      p.status        = status
      p.instrument    = instrument
      p.targetName    = targetName
      p.name          = name
      p.obsClass      = obsClass
      p.active        = active
      p.nextStepToRun = nextStepToRun
      p.runningStep   = runningStep
      p.loaded        = loaded
      p.inDayCalQueue = inDayCalQueue
      p
    }

    def unapply(l: SessionQueueRow):
      Option[(Observation.Id,
       SequenceState,
       Instrument,
       Option[String],
       String,
       ObsClass,
       Boolean,
       Boolean,
       Option[Int],
       Option[RunningStep],
       Boolean)] =
      Some(
        (l.obsId,
         l.status,
         l.instrument,
         l.targetName,
         l.name,
         l.obsClass,
         l.active,
         l.loaded,
         l.nextStepToRun,
         l.runningStep,
         l.inDayCalQueue))

    def Empty: SessionQueueRow =
      apply(
        Observation.Id.unsafeFromString("Zero-1"),
        SequenceState.Idle,
        Instrument.F2,
        None,
        "",
        ObsClass.Nighttime,
        active = false,
        loaded = false,
        None,
        None,
        inDayCalQueue = false
      )
  }

  private def linkTo(p: Props, page: SeqexecPages)(mod: TagMod*) =
    <.a(
      ^.href := p.ctl.urlFor(page).value,
      ^.onClick ==> { _.preventDefaultCB },
      ^.draggable := false,
      mod.toTagMod
    )

  def pageOf(row: SessionQueueRow): SeqexecPages =
    if (row.loaded) {
      SequencePage(row.instrument,
                   row.obsId,
                   StepIdDisplayed(row.nextStepToRun.getOrElse(0)))
    } else {
      PreviewPage(row.instrument,
                  row.obsId,
                  StepIdDisplayed(row.nextStepToRun.getOrElse(0)))
    }

  private def linkedTextRenderer(p: Props)(
    f:                              SessionQueueRow => TagMod
  ): CellRenderer[js.Object, js.Object, SessionQueueRow] =
    (_, _, _, row: SessionQueueRow, _) => {
      linkTo(p, pageOf(row))(SeqexecStyles.queueTextColumn, f(row))
    }

  private def obsIdRenderer(p: Props) = linkedTextRenderer(p) { r =>
    <.p(SeqexecStyles.queueText, r.obsId.format)
  }

  private def obsNameRenderer(p: Props) = linkedTextRenderer(p) { r =>
    <.p(SeqexecStyles.queueText, r.name)
  }

  private def statusText(status:      SequenceState,
                         runningStep: Option[RunningStep]): String =
    s"${status.show} ${runningStep.map(u => s" ${u.show}").getOrElse("")}"

  private def stateRenderer(p: Props) = linkedTextRenderer(p) { r =>
    <.p(SeqexecStyles.queueText, statusText(r.status, r.runningStep))
  }

  private def instrumentRenderer(p: Props) = linkedTextRenderer(p) { r =>
    <.p(SeqexecStyles.queueText, r.instrument.show)
  }

  private def targetRenderer(p: Props) = linkedTextRenderer(p) { r =>
    val targetName =
      r.targetName.fold(<.span(UnknownTargetName): TagMod)(x => x: TagMod)
    <.p(SeqexecStyles.queueText, targetName)
  }

  private def statusIconRenderer(
    b: Backend
  ): CellRenderer[js.Object, js.Object, SessionQueueRow] =
    (_, _, _, row: SessionQueueRow, index) => {
      val isFocused = row.active
      val icon: TagMod =
        row.status match {
          case SequenceState.Completed =>
            IconCheckmark.copyIcon(
              extraStyles = List(SeqexecStyles.selectedIcon))
          case SequenceState.Running(_, _) =>
            IconCircleNotched.copyIcon(fitted  = true,
                                       loading = true,
                                       extraStyles =
                                         List(SeqexecStyles.runningIcon))
          case SequenceState.Failed(_) =>
            IconAttention.copyIcon(color = "red".some,
                                   extraStyles =
                                     List(SeqexecStyles.selectedIcon))
          case _ if b.state.rowLoading.exists(_ === index) =>
            // Spinning icon while loading
            IconRefresh.copyIcon(fitted      = true,
                                 loading     = true,
                                 extraStyles = List(SeqexecStyles.runningIcon))
          case _ if isFocused =>
            IconSelectedRadio.copyIcon(
              extraStyles = List(SeqexecStyles.selectedIcon))
          case _ =>
            <.div()
        }

      linkTo(b.props, pageOf(row))(
        SeqexecStyles.queueIconColumn,
        icon
      )
    }

  private def classIconRenderer(
    b: Backend
  ): CellRenderer[js.Object, js.Object, SessionQueueRow] =
    (_, _, _, row: SessionQueueRow, _) => {
      val icon: TagMod =
        row.obsClass match {
          case ObsClass.Daytime =>
            IconSun.copyIcon(extraStyles = List(SeqexecStyles.selectedIcon))
          case ObsClass.Nighttime =>
            IconMoon.copyIcon(extraStyles = List(SeqexecStyles.selectedIcon))
          case _ =>
            <.div()
        }

      linkTo(b.props, pageOf(row))(
        SeqexecStyles.queueIconColumn,
        icon
      )
    }

  def addToQueueE(id: Observation.Id)(e: ReactEvent): Callback =
    e.stopPropagationCB *>
      e.preventDefaultCB *>
      SeqexecCircuit.dispatchCB(RequestAddSeqCal(CalibrationQueueId, id))

  def removeFromQueueE(id: Observation.Id)(e: ReactEvent): Callback =
    e.stopPropagationCB *>
      e.preventDefaultCB *>
      SeqexecCircuit.dispatchCB(RequestRemoveSeqCal(CalibrationQueueId, id))

  private def addToQueueRenderer(
    b: Backend
  ): CellRenderer[js.Object, js.Object, SessionQueueRow] =
    (_, _, _, row: SessionQueueRow, _) => {
      linkTo(b.props, pageOf(row))(
        SeqexecStyles.queueIconColumn,
        ^.title := (if (row.inDayCalQueue) {
                      "Remove from daycal queue"
                    } else {
                      "Add to daycal queue"
                    }),
        if (row.inDayCalQueue) {
          IconCheckCircleOutline.copyIcon(size   = SSize.Large,
                                          fitted = true,
                                          extraStyles =
                                            List(SeqexecStyles.selectedIcon),
                                          onClickE =
                                            removeFromQueueE(row.obsId) _)
        } else {
          IconCircleOutline.copyIcon(size   = SSize.Large,
                                     fitted = true,
                                     extraStyles =
                                       List(SeqexecStyles.selectedIcon),
                                     onClickE = addToQueueE(row.obsId) _)
        }
      )
    }

  private val statusHeaderRenderer: HeaderRenderer[js.Object] =
    (_, _, _, _, _, _) =>
      <.div(
        ^.title := "Control",
        ^.width := IconColumnWidth.px
    )

  def addAll: Callback =
    SeqexecCircuit.dispatchCB(RequestAllSelectedSequences(CalibrationQueueId))

  private val addHeaderRenderer: HeaderRenderer[js.Object] =
    (_, _, _, _, _, _) =>
      <.div(
        ^.title := "Add all to queue",
        ^.width := (AddQueueColumnWidth - 1).px,
        SeqexecStyles.selectedIcon,
        SeqexecStyles.centeredCell,
        IconCalendarOutline.copyIcon(fitted  = true,
                                     link    = true,
                                     onClick = addAll)
    )

  private val timeHeaderRenderer: HeaderRenderer[js.Object] =
    (_, _, _, _, _, _) =>
      <.div(
        ^.title := "Obs. class",
        ^.width := (ClassColumnWidth - 1).px,
        SeqexecStyles.selectedIcon,
        SeqexecStyles.centeredCell,
        IconClockOutline.copyIcon(fitted = true)
    )

  def rowClassName(p: Props)(i: Int): String =
    ((i, p.rowGetter(i)) match {
      case (-1, _) =>
        SeqexecStyles.headerRowStyle
      case (_, SessionQueueRow(_, s, _, _, _, _, _, _, _, _, _))
          if s == SequenceState.Completed =>
        SeqexecStyles.stepRow |+| SeqexecStyles.draggableRow |+| SeqexecStyles.rowPositive
      case (_, SessionQueueRow(_, s, _, _, _, _, _, _, _, _, _))
          if s.isRunning =>
        SeqexecStyles.stepRow |+| SeqexecStyles.draggableRow |+| SeqexecStyles.rowWarning
      case (_, SessionQueueRow(_, s, _, _, _, _, _, _, _, _, _)) if s.isError =>
        SeqexecStyles.stepRow |+| SeqexecStyles.draggableRow |+| SeqexecStyles.rowNegative
      case (_, SessionQueueRow(_, s, _, _, _, _, _, active, _, _, _))
          if active && !s.isInProcess =>
        SeqexecStyles.stepRow |+| SeqexecStyles.draggableRow |+| SeqexecStyles.rowActive
      case _ =>
        SeqexecStyles.stepRow |+| SeqexecStyles.draggableRow
    }).htmlClass

  // scalastyle:off
  private def columns(b: Backend, size: Size): List[Table.ColumnArg] = {
    val props = b.props

    // Tell the model to resize a column
    def resizeRow(c: TableColumn): (String, JsNumber) => Callback =
      (_, dx) => {
        val percentDelta = dx.toDouble / size.width
        val ns           = b.state.applyOffset(c, percentDelta, size)
        b.setState(ns) *> SeqexecCircuit.dispatchCB(
          UpdateSessionQueueTableState(ns.tableState))
      }

    b.state.visibleColumnsSizes(size).collect {
      case (IconColumn, width, _) =>
        Column(
          Column.propsNoFlex(
            width,
            dataKey        = "status",
            label          = "",
            cellRenderer   = statusIconRenderer(b),
            headerRenderer = statusHeaderRenderer,
            className      = SeqexecStyles.queueIconColumn.htmlClass
          ))
      case (AddQueueColumn, width, _) =>
        Column(
          Column.propsNoFlex(
            width,
            dataKey        = "obsClass",
            label          = "",
            cellRenderer   = addToQueueRenderer(b),
            headerRenderer = addHeaderRenderer,
            className      = SeqexecStyles.queueIconColumn.htmlClass
          ))
      case (ClassColumn, width, _) =>
        Column(
          Column.propsNoFlex(
            width,
            dataKey        = "obsClass",
            label          = "",
            cellRenderer   = classIconRenderer(b),
            headerRenderer = timeHeaderRenderer,
            className      = SeqexecStyles.queueIconColumn.htmlClass
          ))
      case (ObsIdColumn, width, _) =>
        Column(
          Column.propsNoFlex(
            width,
            dataKey        = "obsid",
            label          = "Obs. ID",
            cellRenderer   = obsIdRenderer(props),
            headerRenderer = resizableHeaderRenderer(resizeRow(ObsIdColumn)),
            className      = SessionQueueColumnStyle
          ))
      case (StateColumn, width, _) =>
        Column(
          Column.propsNoFlex(
            width,
            dataKey        = "state",
            label          = "State",
            cellRenderer   = stateRenderer(props),
            headerRenderer = resizableHeaderRenderer(resizeRow(StateColumn)),
            className      = SessionQueueColumnStyle
          ))
      case (InstrumentColumn, width, false) =>
        Column(
          Column.propsNoFlex(
            width,
            dataKey      = "instrument",
            label        = "Instrument",
            cellRenderer = instrumentRenderer(props),
            headerRenderer =
              resizableHeaderRenderer(resizeRow(InstrumentColumn)),
            className = SessionQueueColumnStyle
          ))
      case (InstrumentColumn, width, true) =>
        Column(
          Column.propsNoFlex(
            width,
            dataKey      = "instrument",
            label        = "Instrument",
            cellRenderer = instrumentRenderer(props),
            className    = SessionQueueColumnStyle
          ))
      case (ObsNameColumn, width, _) =>
        Column(
          Column.propsNoFlex(
            width,
            dataKey      = "obsName",
            label        = "Obs. Name",
            cellRenderer = obsNameRenderer(props),
            className    = SessionQueueColumnStyle
          ))
      case (TargetNameColumn, width, _) =>
        Column(
          Column.propsNoFlex(
            width,
            dataKey      = "target",
            label        = "Target",
            cellRenderer = targetRenderer(props),
            headerRenderer =
              resizableHeaderRenderer(resizeRow(TargetNameColumn)),
            className = SessionQueueColumnStyle
          ))
    }
  }
  // scalastyle:on

  def updateScrollPosition(b: Backend, pos: JsNumber): Callback = {
    val s = State.scrollPosition.set(pos)(b.state)
    b.setState(s) *> SeqexecCircuit.dispatchCB(
      UpdateSessionQueueTableState(s.tableState))
  }

  // Single click puts in preview or go to tab
  def singleClick(b: Backend)(i: Int): Callback = {
    val r = b.props.rowGetter(i)
    val p = pageOf(r)
    // If already loaded switch tabs or to preview
    b.props.ctl.setUrlAndDispatchCB(p)
  }

  // Double click tries to load
  def doubleClick(b: Backend)(i: Int): Callback = {
    val r = b.props.rowGetter(i)
    if (r.loaded) {
      // If already loaded switch tabs
      b.props.ctl.dispatchAndSetUrlCB(
        SelectIdToDisplay(r.instrument,
                          r.obsId,
                          StepIdDisplayed(r.nextStepToRun.getOrElse(0))))
    } else { // Try to load it
      b.props.user.filter{_ =>
        b.props.canOperate && i >= 0 && !r.loaded
      }.map { u =>
        val load = SeqexecCircuit.dispatchCB(LoadSequence(Observer(u.displayName), r.instrument, r.obsId))
        val spin = b.modState(_.copy(rowLoading = i.some))
        spin *> load
      }.getOrEmpty
    }
  }

  def table(b: Backend)(size: Size): VdomNode =
    Table(
      Table.props(
        disableHeader = false,
        noRowsRenderer = () =>
          <.div(
            ^.cls := "ui center aligned segment noRows",
            SeqexecStyles.noRowsSegment,
            ^.height := 180.px,
            "Session queue empty"
        ),
        overscanRowCount = SeqexecStyles.overscanRowCount,
        height           = 180,
        rowCount         = b.props.rowCount,
        rowHeight        = SeqexecStyles.rowHeight,
        rowClassName     = rowClassName(b.props) _,
        width            = size.width.toInt,
        rowGetter        = b.props.rowGetter _,
        headerClassName  = SeqexecStyles.tableHeader.htmlClass,
        scrollTop        = b.state.tableState.scrollPosition,
        onScroll         = (_, _, pos) => updateScrollPosition(b, pos),
        onRowDoubleClick = doubleClick(b),
        onRowClick       = singleClick(b),
        headerHeight     = SeqexecStyles.headerHeight,
        rowRenderer      = draggableRowRenderer(b)
      ),
      columns(b, size): _*
    ).vdomElement

  def dragStart(b: Backend, obsId: Observation.Id)(
    e:             ReactDragEvent): Callback =
    Callback {
      e.dataTransfer.setData("text/plain", obsId.format)
    }.when(b.props.canOperate) *> Callback.empty

  private def draggableRowRenderer(b: Backend) =
    (className:        String,
     columns:          Array[VdomNode],
     index:            Int,
     _:                Boolean,
     key:              String,
     rowData:          SessionQueueRow,
     onRowClick:       Option[OnRowClick],
     onRowDoubleClick: Option[OnRowClick],
     _:                Option[OnRowClick],
     _:                Option[OnRowClick],
     _:                Option[OnRowClick],
     style:            Style) => {
      <.div(
        ^.cls := className,
        ^.draggable := b.props.canOperate,
        ^.key := key,
        ^.role := "row",
        ^.onDragStart ==> dragStart(b, rowData.obsId),
        ^.style := Style.toJsObject(style),
        ^.onClick -->? onRowClick.map(h => h(index)),
        ^.onDoubleClick -->? onRowDoubleClick.map(h => h(index)),
        columns.toTagMod
      ): VdomElement
    }

  def initialState(p: Props): State =
    InitialTableState
      .copy(tableState = p.startState)
      .loginState(p.canOperate)
      .withWidths(p.sequencesList)

  private val component = ScalaComponent
    .builder[Props]("SessionQueueTable")
    .initialStateFromProps(initialState)
    .render($ => AutoSizer(AutoSizer.props(table($), disableHeight = true)))
    .configure(Reusability.shouldComponentUpdate)
    .componentWillReceiveProps { $ =>
      $.modState { s =>
        s.loginState($.nextProps.canOperate)
          .withWidths($.nextProps.sequencesList)
          .resetLoading($.nextProps)
      }
    }
    .build

  def apply(
    ctl: RouterCtl[SeqexecPages],
    p:   StatusAndLoadedSequencesFocus
  ): Unmounted[Props, State, Unit] =
    component(Props(ctl, p))

}
