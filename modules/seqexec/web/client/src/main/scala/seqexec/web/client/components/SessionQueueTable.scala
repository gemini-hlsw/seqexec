// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import monocle.Lens
import monocle.macros.GenLens
import react.virtualized._
import scala.math.max
import scala.scalajs.js
import react.common._
import react.common.syntax._
import seqexec.model.enum.Instrument
import seqexec.model.UserDetails
import seqexec.model.UnknownTargetName
import seqexec.model.Observer
import seqexec.model.SequenceState
import seqexec.model.CalibrationQueueId
import seqexec.model.RunningStep
import seqexec.web.client.circuit._
import seqexec.web.client.actions._
import seqexec.web.client.model.Pages._
import seqexec.web.client.model.ObsClass
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
  private val ObsIdMinWidth         = 72.2667 + SeqexecStyles.TableBorderWidth // Measured valu e
  private val StateColumnWidth      = 80.0
  private val StateMinWidth         = 53.3667 + SeqexecStyles.TableBorderWidth
  private val InstrumentColumnWidth = 80.0
  private val InstrumentMinWidth    = 90.4333 + SeqexecStyles.TableBorderWidth
  private val TargetNameColumnWidth = 140.0
  private val TargetMinWidth        = 60.0167 + SeqexecStyles.TableBorderWidth
  private val ObsNameColumnWidth    = 140.0
  private val ObsNameMinWidth       = 60.0167 + SeqexecStyles.TableBorderWidth

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
    VariableColumnWidth.unsafeFromDouble(0.1, ObsIdMinWidth))

  val StateColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    StateColumn,
    name    = "state",
    label   = "State",
    visible = true,
    VariableColumnWidth.unsafeFromDouble(0.1, StateMinWidth))

  val InstrumentColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    InstrumentColumn,
    name    = "instrument",
    label   = "Instrument",
    visible = true,
    VariableColumnWidth.unsafeFromDouble(0.3, InstrumentMinWidth))

  val TargetNameColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    TargetNameColumn,
    name    = "target",
    label   = "Target",
    visible = true,
    VariableColumnWidth.unsafeFromDouble(0.25, TargetMinWidth))

  val ObsNameColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    ObsNameColumn,
    name    = "obsName",
    label   = "Obs. Name",
    visible = true,
    VariableColumnWidth.unsafeFromDouble(0.25, ObsNameMinWidth))

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
    def loginState(canOperate: Boolean): State = {
      val loginChanged = canOperate =!= loggedIn
      (State.userModified.modify(m =>
        UserModified.fromBool((m === IsModified) && !loginChanged)) >>> State.loggedIn.set(canOperate))(this)
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
        println(sequences)
        this
        // State.tableState.modify(_.normalizeWeights(colWidths(sequences)))(this)
      }

    // Hide some columns depending on width
    def visibleColsFor(s: Size, t: TableColumn): Boolean =
      s.width match {
        case w if w < PhoneCut =>
        t match {
            case ObsNameColumn =>
              false
            case TargetNameColumn =>
              false
            case AddQueueColumn =>
              loggedIn
              case _ =>
              true
          }
        case w if w < LargePhoneCut =>
        t match {
            case TargetNameColumn =>
              false
            case ObsNameColumn | AddQueueColumn =>
              loggedIn
            case _=>
              true
          }
        case _ =>
          t match {
            case TargetNameColumn | ObsNameColumn | AddQueueColumn =>
              loggedIn
              case _ =>
          true
        }
      }
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

  // Reusability
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

    def colWidths(sequences: List[SequenceInSessionQueue]): Map[TableColumn, Double] =
      // if (tableState.userModified === IsModified) {
      //   this
      // } else {
        sequences.foldLeft(columnsDefaultWidth) {
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


  private def renderer(c: TableColumn, b: Backend) = c match {
    case IconColumn => statusIconRenderer(b)
    case AddQueueColumn => addToQueueRenderer(b)
    case ClassColumn =>classIconRenderer(b)
    case ObsIdColumn => obsIdRenderer(b.props)
    case StateColumn => stateRenderer(b.props)
    case InstrumentColumn => instrumentRenderer(b.props)
    case TargetNameColumn =>targetRenderer(b.props)
    case ObsNameColumn =>obsNameRenderer(b.props)
  }

  private def fixedHeaderRenderer(c: TableColumn) = c match {
    case IconColumn => statusHeaderRenderer
    case AddQueueColumn => addHeaderRenderer
    case ClassColumn => timeHeaderRenderer
    case _ => defaultHeaderRendererS
  }

  private def columnStyle(c: TableColumn): Option[GStyle] =
    c match {
    case ObsIdColumn | StateColumn | InstrumentColumn | ObsNameColumn |  TargetNameColumn => SeqexecStyles.queueTextColumn.some
    case _ => SeqexecStyles.queueIconColumn.some

    }

  def updateScrollPosition(b: Backend, pos: JsNumber): Callback = {
    val s = State.scrollPosition.set(pos)(b.state)
    b.setState(s) *> SeqexecCircuit.dispatchCB(
      UpdateSessionQueueTableState(s.tableState))
  }

  // scalastyle:off
  private def colBuilder(b: Backend, size: Size): ColumnRenderArgs[TableColumn] => Table.ColumnArg = tb => {
    // val props = b.props

  def updateScrollPosition(ts: TableState[TableColumn]): Callback = {
    val s = State.tableState.set(ts)(b.state)
    b.setState(s) *> SeqexecCircuit.dispatchCB(
      UpdateSessionQueueTableState(s.tableState))
  }

    // Tell the model to resize a column
    // def resizeRow(c: TableColumn): (String, JsNumber) => Callback =
    //   (_, dx) => {
    //     val ns           = b.state.applyOffset(c, dx.toDouble, size)
    //     b.setState(ns) *> SeqexecCircuit.dispatchCB(
    //       UpdateSessionQueueTableState(ns.tableState))
    //   }

    // implicit val s: cats.Show[ColumnWidth] = cats.Show.show[ColumnWidth] { x => x match {
    //   case f: FixedColumnWidth => s"${f.width}px"
    //   case p: VariableColumnWidth => f"${p.width.orEmpty}%.2f, ${p.minWidth}px"
    // }}
    //
    println("BUILD cols")
    println(tb)
    tb match {

        case ColumnRenderArgs(ColumnMeta(c, name, label, _, _),
                              _,
                              width,
                              true) =>
          Column(
            Column.propsNoFlex(
              width        = width,
              dataKey      = name,
              label        = label,
              cellRenderer = renderer(c, b),
              headerRenderer = resizableHeaderRenderer(
                b.state.tableState.resizeRow(c, size, updateScrollPosition)),
              className = columnStyle(c).foldMap(_.htmlClass)
            ))
        case ColumnRenderArgs(ColumnMeta(c, name, label, _, _),
                              _,
                              width,
                              false) =>
          Column(
            Column.propsNoFlex(width        = width,
                               dataKey      = name,
                               label        = label,
                               headerRenderer = fixedHeaderRenderer(c),
                               cellRenderer = renderer(c, b),
              className = columnStyle(c).foldMap(_.htmlClass)
            ))
    }
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
      b.state.tableState.columnBuilder(size, b.state.visibleColsFor(_, _), colBuilder(b, size)): _*
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
