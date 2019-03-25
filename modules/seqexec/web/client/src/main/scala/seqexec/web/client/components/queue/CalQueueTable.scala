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
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.extra.TimerSupport
import monocle.macros.Lenses
import react.virtualized._
import react.sortable._
import scala.scalajs.js
import scala.concurrent.duration._
import react.common._
import seqexec.model.QueueId
import seqexec.model.enum.Instrument
import seqexec.model.enum.QueueManipulationOp
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
import seqexec.web.client.semanticui.{ Size => SSize }
import web.client.table._

/**
  * Calibration queue table
  */
object CalQueueTable {
  sealed trait TableColumn extends Product with Serializable
  case object RemoveSeqColumn extends TableColumn
  case object ObsIdColumn extends TableColumn
  case object InstrumentColumn extends TableColumn

  private val RemoveColumnWidth  = 30.0
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

  val ObsIdColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    ObsIdColumn,
    name    = "obsid",
    label   = "Obs. ID",
    visible = true,
    PercentageColumnWidth.unsafeFromDouble(0.5, ObsIdMinWidth))

  val InstrumentColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    InstrumentColumn,
    name    = "instrument",
    label   = "Instrument",
    visible = true,
    PercentageColumnWidth.unsafeFromDouble(0.5, InstrumentMinWidth))

  val all: NonEmptyList[ColumnMeta[TableColumn]] =
    NonEmptyList.of(RemoveSeqMeta, ObsIdColumnMeta, InstrumentColumnMeta)

  val ro: NonEmptyList[ColumnMeta[TableColumn]] =
    NonEmptyList.of(ObsIdColumnMeta, InstrumentColumnMeta)

  final case class Props(queueId: QueueId, data: CalQueueFocus) {
    val rowCount: Int = data.seqs.size

    val canOperate: Boolean = data.canOperate

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
          CalQueueRow(s.id, s.i)
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
        .map(_._2)
        .map { i =>
          (i to rowCount).toList
        }
        .getOrElse(Nil)

    val upLifted: List[Int] =
      data.seqs.zipWithIndex
        .find {
          case (s, _) =>
            seqState(s.id).exists(
              _.removeSeqQueue === RemoveSeqQueue.RemoveSeqQueueInFlight)
        }
        .map(_._2)
        .map { i =>
          ((i + 1) to rowCount).toList
        }
        .getOrElse(Nil)

  }

  @Lenses
  final case class State(tableState:        TableState[TableColumn],
                         animationRendered: Boolean,
                         moved:             Option[IndexChange])

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object State {
    val EditableTableState: TableState[TableColumn] =
      TableState(NotModified, 0, all)
    val ROTableState: TableState[TableColumn] =
      TableState(NotModified, 0, ro)
    val DefaultRO: State =
      State(ROTableState, animationRendered = false, moved = None)
    val DefaultEditable: State =
      State(EditableTableState, animationRendered = false, moved = None)
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]
  implicit val icReuse: Reusability[IndexChange] =
    Reusability.derive[IndexChange]
  implicit val stateReuse: Reusability[State] =
    Reusability.by(x => (x.tableState, x.moved))

  // ScalaJS defined trait
  // scalastyle:off
  trait CalQueueRow extends js.Object {
    var obsId: Observation.Id
    var instrument: Instrument
  }
  // scalastyle:on

  object CalQueueRow {

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def apply(obsId: Observation.Id, instrument: Instrument): CalQueueRow = {
      val p = (new js.Object).asInstanceOf[CalQueueRow]
      p.obsId      = obsId
      p.instrument = instrument
      p
    }

    def unapply(l: CalQueueRow): Option[(Observation.Id, Instrument)] =
      Some((l.obsId, l.instrument))

    def Empty: CalQueueRow =
      apply(Observation.Id.unsafeFromString("Default-1"), Instrument.F2)
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
        ^.width := 100.pct,
        ^.height := 100.pct,
        Button(
          Button.Props(
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
          ))
    )

  class CalQueueTableBackend(b: BackendScope[Props, State])
      extends TimerSupport {

    private def colBuilder(
      props: Props,
      state: State,
      size:  Size
    ): ColumnRenderArgs[TableColumn] => Table.ColumnArg = tb => {
      def updateState(s: TableState[TableColumn]): Callback =
        b.modState(State.tableState.set(s)) *>
          SeqexecCircuit.dispatchCB(UpdateCalTableState(props.queueId, s))

      def renderer(c: TableColumn) = c match {
        case RemoveSeqColumn  => removeSeqRenderer(props)
        case ObsIdColumn      => obsIdRenderer
        case InstrumentColumn => instrumentRenderer
      }

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
              cellRenderer = renderer(c),
              headerRenderer = resizableHeaderRenderer(
                state.tableState.resizeRow(c, size, updateState)),
              className = SeqexecStyles.queueTextColumn.htmlClass
            ))
        case ColumnRenderArgs(ColumnMeta(c, name, label, _, _),
                              _,
                              width,
                              false) =>
          Column(
            Column.propsNoFlex(width        = width,
                               dataKey      = name,
                               label        = label,
                               cellRenderer = renderer(c),
                               className =
                                 if (c === InstrumentColumn)
                                   SeqexecStyles.queueTextColumn.htmlClass
                                 else "")
          )
      }
    }

    def rowClassName(p: Props, s: State)(i: Int): String =
      ((i, p.rowGetter(s)(i)) match {
        case (-1, _) =>
          SeqexecStyles.headerRowStyle
        case (_, CalQueueRow(i, _))
            if p.addedRows.contains(i) && !s.animationRendered =>
          SeqexecStyles.stepRow |+| SeqexecStyles.draggableRow |+| SeqexecStyles.calRowBackground
        case (_, CalQueueRow(i, _))
            if p.movedRows.contains(i) && !s.animationRendered =>
          SeqexecStyles.stepRow |+| SeqexecStyles.draggableRow |+| SeqexecStyles.calRowBackground
        case (i, CalQueueRow(_, _))
            if p.afterDeletedRows.contains(i) && !s.animationRendered =>
          SeqexecStyles.stepRow |+| SeqexecStyles.draggableRow |+| SeqexecStyles.calRowBackground
        case (i, _) if p.upLifted.contains(i) && !s.animationRendered =>
          SeqexecStyles.stepRow |+| SeqexecStyles.draggableRow |+| SeqexecStyles.deletedRow
        case _ =>
          SeqexecStyles.stepRow |+| SeqexecStyles.draggableRow
      }).htmlClass

    def updateScrollPosition(pos: JsNumber): Callback =
      b.props.zip(b.state) >>= {
        case (p, state) =>
          val s =
            (State.tableState ^|-> TableState.scrollPosition).set(pos)(state)
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
        height           = size.height.toInt,
        rowCount         = p.rowCount,
        rowHeight        = SeqexecStyles.rowHeight,
        rowClassName     = rowClassName(p, s) _,
        width            = size.width.toInt,
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
      b.modState(_.copy(animationRendered = true)) *>
        b.props >>= { p =>
        SeqexecCircuit.dispatchCB(ClearLastQueueOp(p.queueId))
      }

    def allowAnim: Callback =
      b.modState(_.copy(animationRendered = false))

    def resetMoved: Callback =
      b.modState(_.copy(moved = none))

    def updateVisibleCols: Callback =
      b.props >>= { p =>
        val cols = if (p.data.loggedIn) {
          State.DefaultEditable.tableState.columns
        } else {
          State.DefaultRO.tableState.columns
        }
        b.modState(s => s.copy(tableState = s.tableState.copy(columns = cols)))
      }

    def render(p: Props, s: State): VdomElement =
      TableContainer(TableContainer.Props(p.canOperate, size => {
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
      }))

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
      c.backend.resetAnim.when(c.props.data.lastOp.isDefined) *>
        Callback.empty
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
