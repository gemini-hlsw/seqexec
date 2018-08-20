// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import cats.implicits._
import cats.data.NonEmptyList
import cats.Eq
import diode.react.ModelProxy
import gem.Observation
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.raw.JsNumber
import mouse.all._
import monocle.Lens
import monocle.macros.GenLens
import react.virtualized._
import scala.math.max
import scala.scalajs.js
import seqexec.model.enum.Instrument
import seqexec.model.{ UserDetails, DaytimeCalibrationTargetName, Observer, SequenceState }
import seqexec.web.client.circuit._
import seqexec.web.client.actions._
import seqexec.web.client.model.Pages._
import seqexec.web.client.model.RunningStep
import seqexec.web.client.ModelOps._
import seqexec.web.client.semanticui.elements.icon.Icon.IconAttention
import seqexec.web.client.semanticui.elements.icon.Icon.IconCheckmark
import seqexec.web.client.semanticui.elements.icon.Icon.IconCircleNotched
import seqexec.web.client.semanticui.elements.icon.Icon.IconRefresh
import seqexec.web.client.semanticui.elements.icon.Icon.IconSelectedRadio
import web.client.style._
import web.client.utils._
import web.client.table._

object QueueTableBody {
  type Backend = RenderScope[Props, State, Unit]

  private val PhoneCut                 = 400
  private val LargePhoneCut            = 570
  private val IconColumnWidth          = 25
  private val ObsIdColumnWidth         = 140
  private val StateColumnWidth         = 80
  private val InstrumentColumnWidth    = 80
  private val ObsNameColumnWidth       = 140
  private val TargetNameColumnWidth    = 140
  private val QueueColumnStyle: String = SeqexecStyles.queueTextColumn.htmlClass

  sealed trait TableColumn
  case object IconColumn       extends TableColumn
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
    name = "status",
    label = "",
    visible = true,
    FixedColumnWidth(IconColumnWidth))

  val ObsIdColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    ObsIdColumn,
    name = "obsid",
    label = "Obs. ID",
    visible = true,
    PercentageColumnWidth.Full)

  val StateColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    StateColumn,
    name = "state",
    label = "State",
    visible = true,
    PercentageColumnWidth.Full)

  val InstrumentColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    InstrumentColumn,
    name = "instrument",
    label = "Instrument",
    visible = true,
    PercentageColumnWidth.Full)

  val ObsNameColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    ObsNameColumn,
    name = "obsName",
    label = "Obs. Name",
    visible = true,
    PercentageColumnWidth.Full)

  val TargetNameColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    TargetNameColumn,
    name = "target",
    label = "Target",
    visible = true,
    PercentageColumnWidth.Full)

  val all: NonEmptyList[ColumnMeta[TableColumn]] = NonEmptyList.of(
    IconColumnMeta,
    ObsIdColumnMeta,
    StateColumnMeta,
    InstrumentColumnMeta,
    TargetNameColumnMeta,
    ObsNameColumnMeta)

  val columnsDefaultWidth: Map[TableColumn, Int] = Map(
    IconColumn       -> IconColumnWidth,
    ObsIdColumn      -> ObsIdColumnWidth,
    StateColumn      -> StateColumnWidth,
    InstrumentColumn -> InstrumentColumnWidth,
    TargetNameColumn -> TargetNameColumnWidth,
    ObsNameColumn    -> ObsNameColumnWidth
  )

  final case class Props(ctl: RouterCtl[SeqexecPages],
                         sequences: ModelProxy[StatusAndLoadedSequencesFocus]) {
    val sequencesList: List[SequenceInQueue] = sequences().sequences

    val startState: TableState[TableColumn] = sequences().tableState

    def rowGetter(i: Int): QueueRow =
      sequencesList
        .lift(i)
        .map { s =>
          QueueRow(s.id,
                   s.status,
                   s.instrument,
                   s.targetName,
                   s.name,
                   s.active,
                   s.loaded,
                   s.runningStep)
        }
        .getOrElse(QueueRow.Empty)

    val rowCount: Int =
      sequencesList.size

    val canOperate: Boolean = sequences().status.canOperate

    val user: Option[UserDetails] = sequences().status.u
  }

  final case class State(tableState: TableState[TableColumn],
                         rowLoading: Option[Int],
                         loggedIn: Boolean) {
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
          case c @ ColumnMeta(ObsNameColumn, _, _, _, _)    =>
            c.copy(visible = false)
          case c @ ColumnMeta(TargetNameColumn, _, _, _, _) =>
            c.copy(visible = false)
          case c                                            =>
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
    def withWidths(sequences: List[SequenceInQueue]): State =
      if (tableState.userModified === IsModified) {
        this
      } else {
        val optimalSizes = sequences.foldLeft(columnsDefaultWidth) {
          case (currWidths, SequenceInQueue(id, st, i, _, _, n, t, r)) =>
            val idWidth = max(currWidths.getOrElse(ObsIdColumn, 0),
                              tableTextWidth(id.format))
            val statusWidth =
              max(currWidths.getOrElse(StateColumn, 0),
                  tableTextWidth(statusText(st, r)))
            val instrumentWidth =
              max(currWidths.getOrElse(InstrumentColumn, 0),
                  tableTextWidth(i.show))
            val obsNameWidth =
              max(currWidths.getOrElse(ObsNameColumn, 0), tableTextWidth(n))
            val targetNameWidth =
              max(currWidths.getOrElse(TargetNameColumn, 0),
                  tableTextWidth(t.getOrElse("")))

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
          .sum
        // Normalize based on visibility
        State.columns.modify(_.map {
          case c @ ColumnMeta(t, _, _, true, PercentageColumnWidth(_)) =>
            PercentageColumnWidth.fromDouble(optimalSizes.getOrElse(t, 0).toDouble / width)
              .fold(c)(w => c.copy(width = w))
          case c                                                       =>
            c
        })(this)
      }

    // Returns a list of the visible columns with the suggested size
    def visibleColumnsSizes(s: Size): List[(TableColumn, Double, Boolean)] =
      for {
        (c, i) <- hideOnWidth(s).tableState.columns.toList.zipWithIndex
        if c.visible
      } yield
        (c.column,
         tableState.widthOf(c.column, s),
         i === tableState.columns.filter(_.visible).length - 1)

    // Hide some columns depending on width
    private def hideOnWidth(s: Size): State =
      s.width match {
        case w if w < PhoneCut      =>
          State.columns.modify(_.map {
            case c @ ColumnMeta(ObsNameColumn, _, _, _, _)    =>
              c.copy(visible = false)
            case c @ ColumnMeta(TargetNameColumn, _, _, _, _) =>
              c.copy(visible = false)
            case c                                            =>
              c
          })(this)
        case w if w < LargePhoneCut =>
          State.columns.modify(_.map {
            case c @ ColumnMeta(TargetNameColumn, _, _, _, _) =>
              c.copy(visible = false)
            case c                                            =>
              c
          })(this)
        case _                      =>
          this
      }

    def applyOffset(column: TableColumn, delta: Double): State =
      State.tableState.modify(_.applyOffset(column, delta))(this)
  }

  val InitialTableState: State = State(TableState(NotModified, 0, all), None, false)

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

  // ScalaJS defined trait
  // scalastyle:off
  trait QueueRow extends js.Object {
    var obsId: Observation.Id
    var status: SequenceState
    var instrument: Instrument
    var targetName: Option[String]
    var name: String
    var active: Boolean
    var loaded: Boolean
    var runningStep: Option[RunningStep]

  }

  // scalastyle:on
  object QueueRow {

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def apply(obsId: Observation.Id,
              status: SequenceState,
              instrument: Instrument,
              targetName: Option[String],
              name: String,
              active: Boolean,
              loaded: Boolean,
              runningStep: Option[RunningStep]): QueueRow = {
      val p = (new js.Object).asInstanceOf[QueueRow]
      p.obsId = obsId
      p.status = status
      p.instrument = instrument
      p.targetName = targetName
      p.name = name
      p.active = active
      p.runningStep = runningStep
      p.loaded = loaded
      p
    }

    def unapply(l: QueueRow): Option[(Observation.Id,
                                      SequenceState,
                                      Instrument,
                                      Option[String],
                                      String,
                                      Boolean,
                                      Boolean,
                                      Option[RunningStep])] =
      Some(
        (l.obsId,
         l.status,
         l.instrument,
         l.targetName,
         l.name,
         l.active,
         l.loaded,
         l.runningStep))

    def Empty: QueueRow =
      apply(Observation.Id.unsafeFromString("Zero-1"),
            SequenceState.Idle,
            Instrument.F2,
            None,
            "",
            active = false,
            loaded = false,
            None)
  }

  private def linkTo(p: Props, page: SeqexecPages)(mod: TagMod*) =
    <.a(
      ^.href := p.ctl.urlFor(page).value,
      ^.onClick ==> {_.preventDefaultCB},
      mod.toTagMod
    )

  def pageOf(row: QueueRow): SeqexecPages =
    if (row.loaded) {
      SequencePage(row.instrument, row.obsId, 0)
    } else {
      PreviewPage(row.instrument, row.obsId, 0)
    }

  private def linkedTextRenderer(p: Props)(
      f: QueueRow => TagMod): CellRenderer[js.Object, js.Object, QueueRow] =
    (_, _, _, row: QueueRow, _) => {
      linkTo(p, pageOf(row))(SeqexecStyles.queueTextColumn, f(row))
    }

  private def obsIdRenderer(p: Props) = linkedTextRenderer(p) { r =>
    <.p(SeqexecStyles.queueText, r.obsId.format)
  }

  private def obsNameRenderer(p: Props) = linkedTextRenderer(p) { r =>
    <.p(SeqexecStyles.queueText, r.name)
  }

  private def statusText(status: SequenceState,
                         runningStep: Option[RunningStep]): String =
    s"${status.show} ${runningStep.map(u => s" ${u.show}").getOrElse("")}"

  private def stateRenderer(p: Props) = linkedTextRenderer(p) { r =>
    <.p(SeqexecStyles.queueText, statusText(r.status, r.runningStep))
  }

  private def instrumentRenderer(p: Props) = linkedTextRenderer(p) { r =>
    <.p(SeqexecStyles.queueText, r.instrument.show)
  }

  private val daytimeCalibrationTargetName: TagMod =
    <.span(SeqexecStyles.daytimeCal, DaytimeCalibrationTargetName)

  private def targetRenderer(p: Props) = linkedTextRenderer(p) { r =>
    val targetName =
      r.targetName.fold(daytimeCalibrationTargetName)(x => x: TagMod)
    <.p(SeqexecStyles.queueText, targetName)
  }

  private def statusIconRenderer(b: Backend): CellRenderer[js.Object, js.Object, QueueRow] =
    (_, _, _, row: QueueRow, index) => {
      val isFocused = row.active
      val icon: TagMod =
        row.status match {
          case SequenceState.Completed     =>
            IconCheckmark.copyIcon(fitted = true,
                                   extraStyles = List(SeqexecStyles.selectedIcon))
          case SequenceState.Running(_, _) =>
            IconCircleNotched.copyIcon(fitted = true,
                                       loading = true,
                                       extraStyles = List(SeqexecStyles.runningIcon))
          case SequenceState.Failed(_)     =>
            IconAttention.copyIcon(fitted = true, color = "red".some,
                                   extraStyles = List(SeqexecStyles.selectedIcon))
          case _ if b.state.rowLoading.exists(_ === index) =>
            // Spinning icon while loading
            IconRefresh.copyIcon(fitted = true, loading = true,
                                       extraStyles = List(SeqexecStyles.runningIcon))
          case _ if isFocused              =>
            IconSelectedRadio.copyIcon(fitted = true,
                                         extraStyles = List(SeqexecStyles.selectedIcon))
          case _                           =>
            <.div()
        }

      linkTo(b.props, pageOf(row))(
        SeqexecStyles.queueIconColumn,
        icon
      )
    }

  private val statusHeaderRenderer: HeaderRenderer[js.Object] =
    (_, _, _, _, _, _) =>
      <.div(
        ^.title := "Control",
        ^.width := IconColumnWidth.px
    )

  def rowClassName(p: Props)(i: Int): String =
    ((i, p.rowGetter(i)) match {
      case (-1, _) =>
        SeqexecStyles.headerRowStyle
      case (_, QueueRow(_, s, _, _, _, _, _, _)) if s == SequenceState.Completed =>
        SeqexecStyles.stepRow |+| SeqexecStyles.rowPositive
      case (_, QueueRow(_, s, _, _, _, _, _, _)) if s.isRunning                  =>
        SeqexecStyles.stepRow |+| SeqexecStyles.rowWarning
      case (_, QueueRow(_, s, _, _, _, _, _, _)) if s.isError                    =>
        SeqexecStyles.stepRow |+| SeqexecStyles.rowNegative
      case (_, QueueRow(_, s, _, _, _, _, active, _))
          if active && !s.isInProcess                                         =>
        SeqexecStyles.stepRow |+| SeqexecStyles.rowActive
      case _                                                                  =>
        SeqexecStyles.stepRow
    }).htmlClass

  // scalastyle:off
  private def columns(b: Backend, size: Size): List[Table.ColumnArg] = {
    val props = b.props

    // Tell the model to resize a column
    def resizeRow(c: TableColumn): (String, JsNumber) => Callback =
      (_, dx) => {
        val percentDelta = dx.toDouble / size.width
        val ns           = b.state.applyOffset(c, percentDelta)
        b.setState(ns) *> SeqexecCircuit.dispatchCB(
          UpdateQueueTableState(ns.tableState))
      }

    b.state.visibleColumnsSizes(size).collect {
      case (IconColumn, width, _) =>
        Column(
          Column.propsNoFlex(
            width,
            dataKey = "status",
            label = "",
            cellRenderer = statusIconRenderer(b),
            headerRenderer = statusHeaderRenderer,
            className = SeqexecStyles.queueIconColumn.htmlClass
          ))
      case (ObsIdColumn, width, _) =>
        Column(
          Column.propsNoFlex(
            width,
            dataKey = "obsid",
            label = "Obs. ID",
            cellRenderer = obsIdRenderer(props),
            headerRenderer = resizableHeaderRenderer(resizeRow(ObsIdColumn)),
            className = QueueColumnStyle
          ))
      case (StateColumn, width, _) =>
        Column(
          Column.propsNoFlex(
            width,
            dataKey = "state",
            label = "State",
            cellRenderer = stateRenderer(props),
            headerRenderer = resizableHeaderRenderer(resizeRow(StateColumn)),
            className = QueueColumnStyle
          ))
      case (InstrumentColumn, width, false) =>
        Column(
          Column.propsNoFlex(
            width,
            dataKey = "instrument",
            label = "Instrument",
            cellRenderer = instrumentRenderer(props),
            headerRenderer =
              resizableHeaderRenderer(resizeRow(InstrumentColumn)),
            className = QueueColumnStyle
          ))
      case (InstrumentColumn, width, true) =>
        Column(
          Column.propsNoFlex(
            width,
            dataKey = "instrument",
            label = "Instrument",
            cellRenderer = instrumentRenderer(props),
            className = QueueColumnStyle
          ))
      case (ObsNameColumn, width, _) =>
        Column(
          Column.propsNoFlex(
            width,
            dataKey = "obsName",
            label = "Obs. Name",
            cellRenderer = obsNameRenderer(props),
            className = QueueColumnStyle
          ))
      case (TargetNameColumn, width, _) =>
        Column(
          Column.propsNoFlex(
            width,
            dataKey = "target",
            label = "Target",
            cellRenderer = targetRenderer(props),
            headerRenderer =
              resizableHeaderRenderer(resizeRow(TargetNameColumn)),
            className = QueueColumnStyle
          ))
    }
  }
  // scalastyle:on

  def updateScrollPosition(b: Backend, pos: JsNumber): Callback = {
    val s = State.scrollPosition.set(pos)(b.state)
    b.setState(s) *> SeqexecCircuit.dispatchCB(
      UpdateQueueTableState(s.tableState))
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
      b.props.ctl.dispatchAndSetUrlCB(SelectIdToDisplay(r.instrument, r.obsId, 0))
    } else { // Try to load it
      (for {
        u <- b.props.user
        if (b.props.canOperate && i >= 0 && !r.loaded)
      } yield {
        val load = SeqexecCircuit.dispatchCB(LoadSequence(Observer(u.displayName), r.instrument, r.obsId))
        val spin = b.modState(_.copy(rowLoading = i.some))
        spin *> load
      }).getOrEmpty
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
            ^.height := 216.px,
            "Queue empty"
        ),
        overscanRowCount = SeqexecStyles.overscanRowCount,
        height = 216,
        rowCount = b.props.rowCount,
        rowHeight = SeqexecStyles.rowHeight,
        rowClassName = rowClassName(b.props) _,
        width = size.width.toInt,
        rowGetter = b.props.rowGetter _,
        headerClassName = SeqexecStyles.tableHeader.htmlClass,
        scrollTop = b.state.tableState.scrollPosition,
        onScroll = (_, _, pos) => updateScrollPosition(b, pos),
        onRowDoubleClick = doubleClick(b),
        onRowClick = singleClick(b),
        headerHeight = SeqexecStyles.headerHeight
      ),
      columns(b, size): _*
    ).vdomElement

  def initialState(p: Props): State =
    InitialTableState
      .copy(tableState = p.startState)
      .loginState(p.canOperate)
      .withWidths(p.sequencesList)

  private val component = ScalaComponent
    .builder[Props]("QueueTableBody")
    .initialStateFromProps(initialState)
    .render($ => AutoSizer(AutoSizer.props(table($), disableHeight = true)))
    .componentWillReceiveProps { $ =>
      $.modState { s =>
        s.loginState($.nextProps.canOperate).withWidths($.nextProps.sequencesList).resetLoading($.nextProps)
      }
    }
    .build

  def apply(ctl: RouterCtl[SeqexecPages],
            p: ModelProxy[StatusAndLoadedSequencesFocus]): Unmounted[Props, State, Unit] =
    component(Props(ctl, p))

}
