// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.queue

import cats.Eq
import cats.implicits._
import cats.data.NonEmptyList
import gem.Observation
import japgolly.scalajs.react.BackendScope
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.CallbackTo
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.extra.TimerSupport
import japgolly.scalajs.react.MonocleReact._
import monocle.Lens
import monocle.macros.Lenses
import react.virtualized._
import react.sortable._
import react.common._
import react.common.implicits._
import scala.scalajs.js
import scala.math.max
import scala.concurrent.duration._
import seqexec.model.QueueId
import seqexec.model.SequenceState
import seqexec.model.QueueManipulationOp
import seqexec.model.enum.Instrument
import seqexec.web.client.model.QueueSeqOperations
import seqexec.web.client.model.RemoveSeqQueue
import seqexec.web.client.circuit._
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.components.TableContainer
import seqexec.web.client.reusability._
import seqexec.web.client.actions.ClearLastQueueOp
import seqexec.web.client.actions.RequestRemoveSeqCal
import seqexec.web.client.actions.RequestMoveCal
import seqexec.web.client.actions.UpdateCalTableState
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.icon.Icon.IconTimes
import seqexec.web.client.semanticui.elements.icon.Icon.IconRefresh
import seqexec.web.client.semanticui.elements.icon.Icon.IconAttention
import seqexec.web.client.semanticui.elements.icon.Icon.IconCheckmark
import seqexec.web.client.semanticui.elements.icon.Icon.IconCircleNotched
import seqexec.web.client.semanticui.elements.icon.Icon.IconRefresh
import seqexec.web.client.semanticui.{ Size => SSize }
import web.client.table._

/**
  * Calibration queue table
  */
object CalQueueTable {
  sealed trait TableColumn extends Product with Serializable
  case object RemoveSeqColumn extends TableColumn
  case object StateSeqColumn extends TableColumn
  case object ObsIdColumn extends TableColumn
  case object InstrumentColumn extends TableColumn

  private val RemoveColumnWidth  = 30.0
  private val StateColumnWidth   = 25.0
  private val ObsIdMinWidth      = 66.2167 + SeqexecStyles.TableBorderWidth
  private val InstrumentMinWidth = 90.4333 + SeqexecStyles.TableBorderWidth

  object TableColumn {
    implicit val equal: Eq[TableColumn] = Eq.fromUniversalEquals

    implicit val reuse: Reusability[TableColumn] = Reusability.byRef
  }

  val RemoveSeqMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    RemoveSeqColumn,
    name    = "remove",
    label   = "",
    visible = true,
    FixedColumnWidth.unsafeFromDouble(RemoveColumnWidth))

  val StateSeqMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    StateSeqColumn,
    name    = "state",
    label   = "",
    visible = true,
    FixedColumnWidth.unsafeFromDouble(StateColumnWidth))

  val ObsIdColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    ObsIdColumn,
    name    = "obsid",
    label   = "Obs. ID",
    visible = true,
    VariableColumnWidth.unsafeFromDouble(0.5, ObsIdMinWidth))

  val InstrumentColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    InstrumentColumn,
    name    = "instrument",
    label   = "Instrument",
    visible = true,
    VariableColumnWidth.unsafeFromDouble(0.5, InstrumentMinWidth))

  val all: NonEmptyList[ColumnMeta[TableColumn]] =
    NonEmptyList.of(RemoveSeqMeta,
                    StateSeqMeta,
                    ObsIdColumnMeta,
                    InstrumentColumnMeta)

  val ro: NonEmptyList[ColumnMeta[TableColumn]] =
    NonEmptyList.of(StateSeqMeta, ObsIdColumnMeta, InstrumentColumnMeta)

  final case class Props(queueId: QueueId, data: CalQueueFocus) {
    val rowCount: Int = data.seqs.size

    val canOperate: Boolean = data.status.canOperate

    def moveSeq[T: Eq](list: List[T], i: Int, value: T): List[T] = {
      val (front, back) = list.splitAt(i)
      front.filterNot(_ === value) ++ List(value) ++ back.filterNot(_ === value)
    }

    def rowGetter(s: State)(i: Int): CalQueueRow = {
      val moved = s.moved
        .flatMap { c =>
          data.seqs.lift(c.oldIndex).map { o =>
            moveSeq(data.seqs, c.newIndex, o)
          }
        }
        .getOrElse(data.seqs)
      moved
        .lift(i)
        .map { s =>
          CalQueueRow(s.id, s.i, s.status)
        }
        .getOrElse(CalQueueRow.Empty)
    }

    def seqState(id: Observation.Id): Option[QueueSeqOperations] =
      CalQueueFocus.seqQueueOpsT(id).headOption(data)

    val clearOp: Boolean = data.lastOp match {
      case Some(QueueManipulationOp.Clear(_)) => true
      case _                                  => false
    }

    /**
      * Rows added on the last operation
      */
    val addedRows: List[Observation.Id] = data.lastOp match {
      case Some(QueueManipulationOp.AddedSeqs(_, x)) => x
      case _                                         => Nil
    }

    /**
      * Rows deleted on the last operation
      */
    val removedRows: List[Int] = data.lastOp match {
      case Some(QueueManipulationOp.RemovedSeqs(_, _, i)) => i
      case _                                              => Nil
    }

    /**
      * Rows deleted on the last operation
      */
    val movedRows: List[Observation.Id] = data.lastOp match {
      case Some(QueueManipulationOp.Moved(_, _, o, _)) => List(o)
      case _                                           => Nil
    }

    val afterDeletedRows: List[Int] =
      data.seqs.zipWithIndex
        .find {
          case (_, i) => removedRows.contains(i)
        }
        .map { i =>
          (i._2 to rowCount).toList
        }
        .orEmpty

    val upLifted: List[Int] =
      data.seqs.zipWithIndex
        .find {
          case (s, _) =>
            seqState(s.id).exists(
              _.removeSeqQueue === RemoveSeqQueue.RemoveSeqQueueInFlight)
        }
        .map { i =>
          ((i._2 + 1) to rowCount).toList
        }
        .orEmpty

  }

  @Lenses
  final case class State(tableState:        TableState[TableColumn],
                         animationRendered: Boolean,
                         moved:             Option[IndexChange])

  object State {
    val EditableTableState: TableState[TableColumn] =
      TableState(NotModified, 0, all)
    val ROTableState: TableState[TableColumn] =
      TableState(NotModified, 0, ro)
    val DefaultRO: State =
      State(ROTableState, animationRendered = false, moved = None)
    val DefaultEditable: State =
      State(EditableTableState, animationRendered = false, moved = None)

    val scrollPosition: Lens[State, JsNumber] =
      State.tableState ^|-> TableState.scrollPosition

    val columns: Lens[State, NonEmptyList[ColumnMeta[TableColumn]]] =
      State.tableState ^|-> TableState.columns
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]
  implicit val icReuse: Reusability[IndexChange] =
    Reusability.derive[IndexChange]
  implicit val stateReuse: Reusability[State] =
    Reusability.by(x => (x.tableState, x.moved))

  // ScalaJS defined trait
  trait CalQueueRow extends js.Object {
    var obsId: Observation.Id
    var instrument: Instrument
    var status: SequenceState
  }

  object CalQueueRow {

    def apply(obsId:      Observation.Id,
              instrument: Instrument,
              status:     SequenceState): CalQueueRow = {
      val p = (new js.Object).asInstanceOf[CalQueueRow]
      p.obsId      = obsId
      p.instrument = instrument
      p.status     = status
      p
    }

    def unapply(
      l: CalQueueRow): Option[(Observation.Id, Instrument, SequenceState)] =
      Some((l.obsId, l.instrument, l.status))

    def Empty: CalQueueRow =
      apply(Observation.Id.unsafeFromString("Default-1"),
            Instrument.F2,
            SequenceState.Idle)
  }

  val obsIdRenderer: CellRenderer[js.Object, js.Object, CalQueueRow] =
    (_, _, _, r: CalQueueRow, _) => {
      <.p(SeqexecStyles.queueText |+| SeqexecStyles.noselect, r.obsId.format)
    }

  val instrumentRenderer: CellRenderer[js.Object, js.Object, CalQueueRow] =
    (_, _, _, r: CalQueueRow, _) => {
      <.p(SeqexecStyles.queueText |+| SeqexecStyles.noselect, r.instrument.show)
    }

  private def removeSeq(qid: QueueId, sid: Observation.Id): Callback =
    SeqexecCircuit.dispatchCB(RequestRemoveSeqCal(qid, sid))

  def removeSeqRenderer(
    p: Props): CellRenderer[js.Object, js.Object, CalQueueRow] =
    (_, _, _, r: CalQueueRow, _) =>
      <.div(
        SeqexecStyles.centeredCell,
        SeqexecStyles.fullCell,
        Button(
          size        = SSize.Mini,
          basic       = false,
          color       = "brown".some,
          disabled    = !p.data.canOperate,
          compact     = true,
          onClick     = removeSeq(p.queueId, r.obsId),
          extraStyles = List(SeqexecStyles.autoMargin),
          icon = p
            .seqState(r.obsId)
            .filter(
              _.removeSeqQueue === RemoveSeqQueue.RemoveSeqQueueInFlight)
            .fold(IconTimes.copyIcon(
              onClick                      = removeSeq(p.queueId, r.obsId)))(_ =>
              IconRefresh.copyIcon(loading = true))
            .some
        )
    )

  private def statusIconRenderer
    : CellRenderer[js.Object, js.Object, CalQueueRow] =
    (_, _, _, row: CalQueueRow, _) => {
      val selectedIconStyle = SeqexecStyles.selectedIcon
      val icon: TagMod =
        row.status match {
          case SequenceState.Completed =>
            IconCheckmark.copyIcon(extraStyles = List(selectedIconStyle))
          case SequenceState.Running(_, _) =>
            IconCircleNotched.copyIcon(fitted  = true,
                                       loading = true,
                                       extraStyles =
                                         List(SeqexecStyles.runningIcon))
          case SequenceState.Failed(_) =>
            IconAttention.copyIcon(color       = "red".some,
                                   extraStyles = List(selectedIconStyle))
          case _ =>
            EmptyVdom
        }

      <.div(
        SeqexecStyles.centeredCell,
        SeqexecStyles.fullCell,
        icon
      )
    }

  class CalQueueTableBackend(b: BackendScope[Props, State])
      extends TimerSupport {

    private def colBuilder(
      props: Props,
      state: State,
      size:  Size
    ): ColumnRenderArgs[TableColumn] => Table.ColumnArg = tb => {
      def updateState(s: TableState[TableColumn]): Callback =
        b.setStateL(State.tableState)(s) *>
          SeqexecCircuit.dispatchCB(UpdateCalTableState(props.queueId, s))

      def renderer(c: TableColumn) = c match {
        case RemoveSeqColumn  => removeSeqRenderer(props)
        case StateSeqColumn   => statusIconRenderer
        case ObsIdColumn      => obsIdRenderer
        case InstrumentColumn => instrumentRenderer
      }

      tb match {
        case ColumnRenderArgs(meta, _, width, true) =>
          Column(
            Column.propsNoFlex(
              width        = width,
              dataKey      = meta.name,
              label        = meta.label,
              cellRenderer = renderer(meta.column),
              headerRenderer = resizableHeaderRenderer(
                state.tableState.resizeColumn(meta.column, size, updateState)),
              className = SeqexecStyles.queueTextColumn.htmlClass
            ))
        case ColumnRenderArgs(meta, _, width, false) =>
          Column(
            Column.propsNoFlex(
              width        = width,
              dataKey      = meta.name,
              label        = meta.label,
              cellRenderer = renderer(meta.column),
              className =
                if (meta.column === InstrumentColumn)
                  SeqexecStyles.queueTextColumn.htmlClass
                else ""
            )
          )
      }
    }

    private def rowStatusStyle(p: Props, s: State)(i: Int): Css =
      ((i, p.rowGetter(s)(i)) match {
        case (-1, _) =>
          Css.Zero
        case (_, r: CalQueueRow) if r.status === SequenceState.Completed =>
          SeqexecStyles.rowPositive
        case (_, r: CalQueueRow) if r.status.isRunning =>
          SeqexecStyles.rowWarning
        case (_, r: CalQueueRow) if r.status.isError =>
          SeqexecStyles.rowNegative
        case _ =>
          Css.Zero
      })

    def rowClassName(p: Props, s: State)(i: Int): String =
      (((i, p.rowGetter(s)(i)) match {
        case (-1, _) =>
          SeqexecStyles.headerRowStyle
        case (_, CalQueueRow(i, _, _))
            if p.addedRows.contains(i) && !s.animationRendered =>
          SeqexecStyles.stepRow |+| SeqexecStyles.draggableRow |+| SeqexecStyles.calRowBackground
        case (_, CalQueueRow(i, _, _))
            if p.movedRows.contains(i) && !s.animationRendered =>
          SeqexecStyles.stepRow |+| SeqexecStyles.draggableRow |+| SeqexecStyles.calRowBackground
        case (i, CalQueueRow(_, _, _))
            if p.afterDeletedRows.contains(i) && !s.animationRendered =>
          SeqexecStyles.stepRow |+| SeqexecStyles.draggableRow |+| SeqexecStyles.calRowBackground
        case (i, _) if p.upLifted.contains(i) && !s.animationRendered =>
          SeqexecStyles.stepRow |+| SeqexecStyles.draggableRow |+| SeqexecStyles.deletedRow
        case _ =>
          SeqexecStyles.stepRow |+| SeqexecStyles.draggableRow
      }) |+| rowStatusStyle(p, s)(i)).htmlClass

    def updateScrollPosition(pos: JsNumber): Callback =
      b.props.zip(b.state) >>= {
        case (p, state) =>
          val s =
            State.scrollPosition.set(pos)(state)
          b.setState(s) *>
            SeqexecCircuit.dispatchCB(
              UpdateCalTableState(p.queueId, s.tableState))
      }

    private def collapsableStyle: (Int, Style) => Style = (_, s) => s

    def table(p: Props, s: State)(size: Size): Table.Props =
      Table.props(
        disableHeader = false,
        noRowsRenderer = () =>
          <.div(
            ^.cls := "ui center aligned segment noRows",
            SeqexecStyles.noRowsSegment,
            ^.height := size.height.px,
            "Cal queue empty"
        ),
        overscanRowCount = SeqexecStyles.overscanRowCount,
        height           = max(1, size.height.toInt),
        rowCount         = p.rowCount,
        rowHeight        = SeqexecStyles.rowHeight,
        rowClassName     = rowClassName(p, s) _,
        width            = max(1, size.width.toInt),
        rowGetter        = p.rowGetter(s) _,
        headerClassName  = SeqexecStyles.tableHeader.htmlClass,
        scrollTop        = s.tableState.scrollPosition,
        onScroll         = (_, _, pos) => updateScrollPosition(pos),
        rowRenderer      = sortableRowRenderer(collapsableStyle),
        headerHeight     = SeqexecStyles.headerHeight,
        gridClassName =
          if (p.clearOp && !s.animationRendered)
            SeqexecStyles.calTableBorder.htmlClass
          else ""
      )

    def requestMove(c: IndexChange): Callback =
      (b.props >>= { p =>
        p.data.seqs
          .map(_.id)
          .lift(c.oldIndex)
          .map(i =>
            SeqexecCircuit.dispatchCB(
              RequestMoveCal(p.queueId, i, c.newIndex - c.oldIndex)))
          .getOrEmpty
      }) *> b.modState(_.copy(moved = c.some))

    def resetAnim: Callback =
      b.setStateL(State.animationRendered)(true) *>
        b.props >>= { p =>
        SeqexecCircuit.dispatchCB(ClearLastQueueOp(p.queueId))
      }

    def allowAnim: Callback =
      b.setStateL(State.animationRendered)(false)

    def resetMoved: Callback =
      b.setStateL(State.moved)(none)

    def updateVisibleCols: Callback =
      b.props >>= { p =>
        val cols = if (p.data.loggedIn) {
          State.DefaultEditable.tableState.columns
        } else {
          State.DefaultRO.tableState.columns
        }
        b.setStateL(State.columns)(cols)
      }

    def render(p: Props, s: State): VdomElement =
      TableContainer(
        TableContainer.Props(
          p.canOperate,
          size => {
            if (size.width > 0) {
              val sortableList = SortableContainer.wrapC(
                Table.component,
                s.tableState
                  .columnBuilder(size, colBuilder(p, s, size))
                  .map(_.vdomElement))

              // If distance is 0 we can miss some events
              val cp = SortableContainer.Props(
                onSortEnd         = requestMove,
                shouldCancelStart = _ => CallbackTo(!p.data.canOperate),
                helperClass =
                  (SeqexecStyles.noselect |+| SeqexecStyles.draggedRowHelper).htmlClass,
                distance = 3
              )
              sortableList(cp)(table(p, s)(size))
            } else {
              <.div()
            }
          },
          onResize = _ => Callback.empty
        ))

  }

  def initialState(p: Props): State =
    if (p.data.loggedIn) {
      State.DefaultEditable
        .copy(tableState = p.data.tableState)
    } else {
      State.DefaultRO
        .copy(tableState = p.data.tableState)
    }

  private val component = ScalaComponent
    .builder[Props]("CalQueueTable")
    .initialStateFromProps(initialState)
    .renderBackend[CalQueueTableBackend]
    .componentWillMount { c =>
      // If on load we have an op don't animate
      c.backend.resetAnim.when_(c.props.data.lastOp.isDefined)
    }
    .componentWillReceiveProps { c =>
      // This is a bit tricky. if the last op has changed we allow animation
      val opChanged = c.nextProps.data.lastOp =!= c.currentProps.data.lastOp
      c.backend.allowAnim.when(opChanged) *>
        // And then we reset the state to avoid re running the anim
        c.backend.setTimeout(c.backend.resetAnim, 1.second).when(opChanged) *>
        c.backend.resetMoved *>
        c.backend.updateVisibleCols
    }
    .configure(Reusability.shouldComponentUpdate)
    .configure(TimerSupport.install)
    .build

  def apply(p: Props): Unmounted[Props, State, CalQueueTableBackend] =
    component(p)
}
