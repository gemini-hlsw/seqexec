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
import seqexec.web.client.circuit._
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.reusability._
import seqexec.web.client.actions.UpdateCalTableState
import web.client.table._

/**
  * Calibration queue table
  */
object CalQueueTable {
  type Backend = RenderScope[Props, State, Unit]

  sealed trait TableColumn extends Product with Serializable
  case object ObsIdColumn extends TableColumn
  case object InstrumentColumn extends TableColumn

  private val ObsIdMinWidth      = 66.2167 + SeqexecStyles.TableBorderWidth
  private val InstrumentMinWidth = 90.4333 + SeqexecStyles.TableBorderWidth

  object TableColumn {
    implicit val equal: Eq[TableColumn] = Eq.fromUniversalEquals

    implicit val reuse: Reusability[TableColumn] = Reusability.byRef
  }

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
    NonEmptyList.of(ObsIdColumnMeta, InstrumentColumnMeta)

  final case class Props(queueId: QueueId, data: CalQueueFocus) {
    val rowCount: Int = data.seqs.size

    def rowGetter(i: Int): CalQueueRow =
      data.seqs
        .lift(i)
        .map { s =>
          CalQueueRow(s.id, s.i)
        }
        .getOrElse(CalQueueRow.Empty)

    def cmp: Unmounted[js.Object, Null] = {
      val view         = component
      val sortableList = SortableContainer.wrap(view)
      sortableList(SortableContainer.Props())(this)
    }
  }

  @Lenses
  final case class State(tableState: TableState[TableColumn])

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object State {
    val InitialTableState: TableState[TableColumn] =
      TableState(NotModified, 0, all)
    val Zero: State = State(InitialTableState)
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
      apply(Observation.Id.unsafeFromString("Zero-1"), Instrument.F2)
  }

  private val obsIdRenderer: CellRenderer[js.Object, js.Object, CalQueueRow] =
    (_, _, _, r: CalQueueRow, _) => {
      <.p(SeqexecStyles.queueTextColumn, r.obsId.format)
    }

  private val instrumentRenderer: CellRenderer[js.Object, js.Object, CalQueueRow] =
    (_, _, _, r: CalQueueRow, _) => {
      <.p(SeqexecStyles.queueTextColumn, r.instrument.show)
    }

  private def renderer(c: TableColumn) = c match {
    case ObsIdColumn      => obsIdRenderer
    case InstrumentColumn => instrumentRenderer
  }

  private def colBuilder(
    b:    Backend,
    size: Size): ColumnRenderArgs[TableColumn] => Table.ColumnArg = tb => {
    val state = b.state
    def updateState(s: TableState[TableColumn]): Callback =
      b.modState(State.tableState.set(s)) *>
        SeqexecCircuit.dispatchCB(UpdateCalTableState(b.props.queueId, s))

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
                             className    = SeqexecStyles.paddedStepRow.htmlClass))
    }
  }

  def rowClassName(p: Props)(i: Int): String =
    ((i, p.rowGetter(i)) match {
      case (-1, _) =>
        SeqexecStyles.headerRowStyle
      case _ =>
        SeqexecStyles.stepRow
    }).htmlClass

  def updateScrollPosition(b: Backend, pos: JsNumber): Callback = {
    val s = (State.tableState ^|-> TableState.scrollPosition).set(pos)(b.state)
    b.setState(s) *>
      SeqexecCircuit.dispatchCB(UpdateCalTableState(b.props.queueId, s.tableState))
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
        headerHeight     = SeqexecStyles.headerHeight
      ),
      b.state.tableState.columnBuilder(size, colBuilder(b, size)): _*
    ).vdomElement

  def initialState(p: Props): State =
    State.Zero
      .copy(tableState = p.data.tableState)

  private val component = ScalaComponent
    .builder[Props]("CalQueueTable")
    .initialStateFromProps(initialState)
    .renderP { (b, _) =>
      <.div(
        SeqexecStyles.stepsListPaneWithControls,
        AutoSizer(AutoSizer.props(table(b)))
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

}
