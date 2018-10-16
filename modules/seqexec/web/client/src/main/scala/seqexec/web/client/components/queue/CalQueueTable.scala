// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.queue

import cats.Eq
import cats.implicits._
import cats.data.NonEmptyList
import gem.Observation
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Js.Unmounted
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.raw.JsNumber
import monocle.macros.Lenses
import react.virtualized._
import react.sortable._
import scala.scalajs.js
import seqexec.model.QueueId
import seqexec.model.enum.Instrument
import seqexec.model.enum.QueueManipulationOp
import seqexec.web.client.model.QueueSeqOperations
import seqexec.web.client.model.RemoveSeqQueue
import seqexec.web.client.circuit._
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.reusability._
import seqexec.web.client.actions.RequestRemoveSeqCal
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
  type Backend = RenderScope[Props, State, Unit]

  sealed trait TableColumn extends Product with Serializable
  case object RemoveSeqColumn extends TableColumn
  case object ObsIdColumn extends TableColumn
  case object InstrumentColumn extends TableColumn

  private val RemoveColumnWidth  = 34.0
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

    def rowGetter(i: Int): CalQueueRow =
      data.seqs
        .lift(i)
        .map { s =>
          CalQueueRow(s.id, s.i)
        }
        .getOrElse(CalQueueRow.Empty)

    def seqState(id: Observation.Id): Option[QueueSeqOperations] =
      CalQueueFocus.seqQueueOpsT(id).headOption(data)

    val clearOp: Boolean = data.lastOp match {
      case Some(QueueManipulationOp.Clear(_)) => true
      case _                                  => false
    }

    val addedRows: List[Observation.Id] = data.lastOp match {
      case Some(QueueManipulationOp.AddedSeqs(_, x)) => x
      case _                                         => Nil
    }

    println(data.lastOp)
    println(addedRows)

    val cmp: Unmounted[js.Object, Null] = {
      val view         = component
      val sortableList = SortableContainer.wrap(view)
      val p            = SortableContainer.Props()
      sortableList(p)(this)
    }
  }

  @Lenses
  final case class State(tableState: TableState[TableColumn])

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object State {
    val EditableTableState: TableState[TableColumn] =
      TableState(NotModified, 0, all)
    val ROTableState: TableState[TableColumn] =
      TableState(NotModified, 0, ro)
    val DefaultRO: State       = State(ROTableState)
    val DefaultEditable: State = State(EditableTableState)
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]
  implicit val stateReuse: Reusability[State] = Reusability.derive[State]

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
      <.p(SeqexecStyles.queueTextColumn, r.obsId.format)
    }

  val instrumentRenderer: CellRenderer[js.Object, js.Object, CalQueueRow] =
    (_, _, _, r: CalQueueRow, _) => {
      <.p(SeqexecStyles.queueTextColumn, r.instrument.show)
    }

  def removeSeqRenderer(
    p: Props): CellRenderer[js.Object, js.Object, CalQueueRow] =
    (_, _, _, r: CalQueueRow, _) =>
      <.div(
        SeqexecStyles.centeredCell,
        ^.width := 100.pct,
        ^.height := 100.pct,
        Button(
          Button.Props(
            size     = SSize.Mini,
            basic    = false,
            color    = "brown".some,
            disabled = !p.data.canOperate,
            compact  = true,
            onClick =
              SeqexecCircuit.dispatchCB(RequestRemoveSeqCal(p.queueId, r.obsId)),
              extraStyles = List(SeqexecStyles.autoMargin),
            icon = p
              .seqState(r.obsId)
              .filter(_.removeSeqQueue === RemoveSeqQueue.RemoveSeqQueueInFlight)
              .fold(IconTimes)(_ => IconRefresh.copyIcon(loading = true))
              .some
          ))
      )

  private def colBuilder(
    b:    Backend,
    size: Size): ColumnRenderArgs[TableColumn] => Table.ColumnArg = tb => {
    val state = b.state
    def updateState(s: TableState[TableColumn]): Callback =
      b.modState(State.tableState.set(s)) *>
        SeqexecCircuit.dispatchCB(UpdateCalTableState(b.props.queueId, s))

    def renderer(c: TableColumn) = c match {
      case RemoveSeqColumn  => removeSeqRenderer(b.props)
      case ObsIdColumn      => obsIdRenderer
      case InstrumentColumn => instrumentRenderer
    }

    tb match {
      case ColumnRenderArgs(ColumnMeta(c, name, label, _, _), _, width, true) =>
        Column(
          Column.propsNoFlex(
            width        = width,
            dataKey      = name,
            label        = label,
            cellRenderer = renderer(c),
            headerRenderer = resizableHeaderRenderer(
              state.tableState.resizeRow(c, size, updateState)),
            className = SeqexecStyles.paddedStepRow.htmlClass
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
                                 SeqexecStyles.paddedStepRow.htmlClass
                               else "")
        )
    }
  }

  def rowClassName(p: Props)(i: Int): String =
    ((i, p.rowGetter(i)) match {
      case (-1, _) =>
        SeqexecStyles.headerRowStyle
      case (_, CalQueueRow(i, _)) if p.addedRows.contains(i) =>
        SeqexecStyles.stepRow |+| SeqexecStyles.calRowBackground
      case _ =>
        SeqexecStyles.stepRow
    }).htmlClass

  def updateScrollPosition(b: Backend, pos: JsNumber): Callback = {
    val s = (State.tableState ^|-> TableState.scrollPosition).set(pos)(b.state)
    b.setState(s) *>
      SeqexecCircuit.dispatchCB(
        UpdateCalTableState(b.props.queueId, s.tableState))
  }

  def table(b: Backend)(size: Size): VdomNode =
    Table(
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
        rowCount         = b.props.rowCount,
        rowHeight        = SeqexecStyles.rowHeight,
        rowClassName     = rowClassName(b.props) _,
        width            = size.width.toInt,
        rowGetter        = b.props.rowGetter _,
        headerClassName  = SeqexecStyles.tableHeader.htmlClass,
        scrollTop        = b.state.tableState.scrollPosition,
        onScroll         = (_, _, pos) => updateScrollPosition(b, pos),
        rowRenderer      = sortableRowRenderer,
        headerHeight     = SeqexecStyles.headerHeight,
        gridClassName =
          if (b.props.clearOp) SeqexecStyles.calTableBorder.htmlClass else ""
      ),
      b.state.tableState.columnBuilder(size, colBuilder(b, size)): _*
    ).vdomElement

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
    .renderP { (b, _) =>
      <.div(
        SeqexecStyles.stepsListPaneWithControls.when(b.props.canOperate),
        SeqexecStyles.stepsListPanePreview.unless(b.props.canOperate),
        AutoSizer(AutoSizer.props(table(b)))
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

}
