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
import org.scalajs.dom.MouseEvent
import react.virtualized._
import react.draggable._
import scala.math.max
import scala.scalajs.js
import seqexec.model.Model.{
  DaytimeCalibrationTargetName,
  Instrument,
  SequenceState
}
import seqexec.web.client.circuit._
import seqexec.web.client.actions._
import seqexec.web.client.model.Pages._
import seqexec.web.client.ModelOps._
import seqexec.web.client.services.HtmlConstants.iconEmpty
import seqexec.web.client.semanticui.elements.icon.Icon.{
  IconAttention,
  IconCheckmark,
  IconCircleNotched,
  IconSelectedRadio
}
import web.client.style._
import web.client.utils._

object QueueTableBody {
  type $ = RenderScope[Props, TableState, Unit]

  private val PhoneCut                 = 400
  private val LargePhoneCut            = 570
  private val IconColumnWidth          = 20
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

  // Width is either Left[Int] meaning a fixed width or Right[Double] indicating a precentage of the overal width
  final case class ColumnMeta(column: TableColumn, visible: Boolean, width: Either[Int, Double])

  object ColumnMeta {
    val IconColumnMeta: ColumnMeta       = ColumnMeta(IconColumn, visible = true, IconColumnWidth.asLeft)
    val ObsIdColumnMeta: ColumnMeta      = ColumnMeta(ObsIdColumn, visible = true, 1.0.asRight)
    val StateColumnMeta: ColumnMeta      = ColumnMeta(StateColumn, visible = true, 1.0.asRight)
    val InstrumentColumnMeta: ColumnMeta = ColumnMeta(InstrumentColumn, visible = true, 1.0.asRight)
    val ObsNameColumnMeta: ColumnMeta    = ColumnMeta(ObsNameColumn, visible = true, 1.0.asRight)
    val TargetNameColumnMeta: ColumnMeta = ColumnMeta(TargetNameColumn, visible = true, 1.0.asRight)

    val all: NonEmptyList[ColumnMeta] = NonEmptyList.of(IconColumnMeta,
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
  }

  final case class Props(ctl: RouterCtl[SeqexecPages],
                         sequences: ModelProxy[StatusAndLoadedSequencesFocus]) {
    val sequencesList: List[SequenceInQueue] = sequences().sequences

    def rowGetter(i: Int): QueueRow =
      sequencesList
        .lift(i)
        .map { s =>
          QueueRow(s.id, s.status, s.instrument, s.targetName, s.name, s.active, s.runningStep)
        }
        .getOrElse(QueueRow.Zero)

    def rowCount: Int =
      sequencesList.size

    def isLogged: Boolean = sequences().isLogged
  }

  final case class TableState(userModified: Boolean,
                              loggedIn: Boolean,
                              columns: NonEmptyList[ColumnMeta]) {
    // Update the columns' visibility based on logged in state
    private def logIn: TableState =
      copy(loggedIn = true, columns = columns.map(_.copy(visible = true)))

    // Update the columns' visibility based on logged off state
    private def logOff: TableState =
      copy(
        loggedIn = false,
        columns = columns.map {
          case c @ ColumnMeta(ObsNameColumn, _, _)    => c.copy(visible = false)
          case c @ ColumnMeta(TargetNameColumn, _, _) => c.copy(visible = false)
          case c                                      => c
        }
      )

    // Change the columns visibility depending on the logged in state
    def loginState(isLogged: Boolean): TableState = {
      val loginChanged = isLogged =!= loggedIn
      isLogged.fold(logIn, logOff).copy(userModified = userModified && !loginChanged)
    }

    // calculate the relative widths of each column based on content only
    // this should be renormalize against the actual tabel width
    def withWidths(sequences: List[SequenceInQueue]): TableState =
      if (userModified) {
        this
      } else {
        val optimalSizes = sequences.foldLeft(ColumnMeta.columnsDefaultWidth) {
          case (currWidths, SequenceInQueue(id, st, i, _, n, t, r)) =>
            val idWidth = max(currWidths.getOrElse(ObsIdColumn, 0), tableTextWidth(id.format))
            val statusWidth =
              max(currWidths.getOrElse(StateColumn, 0), tableTextWidth(statusText(st, r)))
            val instrumentWidth =
              max(currWidths.getOrElse(InstrumentColumn, 0), tableTextWidth(i.show))
            val obsNameWidth = max(currWidths.getOrElse(ObsNameColumn, 0), tableTextWidth(n))
            val targetNameWidth =
              max(currWidths.getOrElse(TargetNameColumn, 0), tableTextWidth(t.getOrElse("")))
            currWidths + (ObsIdColumn -> idWidth) + (StateColumn -> statusWidth) + (InstrumentColumn -> instrumentWidth) + (ObsNameColumn -> obsNameWidth) + (TargetNameColumn -> targetNameWidth)
        }
        // Width as it would be adding all the visible columns
        val width = optimalSizes
          .filter { case (c, _) => columns.find(_.column === c).forall(_.visible) }
          .values
          .sum
        // Normalize based on visibility
        copy(columns = columns.map {
          case c @ ColumnMeta(t, true, Right(_)) =>
            c.copy(width = (optimalSizes.getOrElse(t, 0).toDouble / width).asRight)
          case c => c
        })
      }

    // Changes the relative widths when a column is being dragged
    def applyOffset(column: TableColumn, delta: Double): TableState = {
      val indexOf = columns.toList.indexWhere(_.column === column)
      // Shift the selected column and the next one
      val result = columns.toList.zipWithIndex.map {
        case (c @ ColumnMeta(_, _, Right(x)), idx) if idx === indexOf     =>
          c.copy(width = (x + delta).asRight)
        case (c @ ColumnMeta(_, _, Right(x)), idx) if idx === indexOf + 1 =>
          c.copy(width = (x - delta).asRight)
        case (c, _)                                                       => c
      }
      copy(userModified = true, columns = NonEmptyList.fromListUnsafe(result))
    }

    // Returns a list of the visible columns with the suggested size
    def visibleColumnsSizes(s: Size): List[(TableColumn, Double, Boolean)] =
      for {
        (c, i) <- hideOnWidth(s: Size).columns.toList.zipWithIndex
        if c.visible
      } yield (c.column, widthOf(c.column, s), i === columns.filter(_.visible).length - 1)

    // Return the width of a column from the actual column width
    def widthOf(column: TableColumn, s: Size): Double =
      columns
        .filter(c => c.column === column && c.visible)
        .map(_.width)
        .map {
          case Left(w)  => w.toDouble
          case Right(f) => f * s.width
        }
        .headOption
        .getOrElse(0.0)

    // Hide some columns depending on width
    private def hideOnWidth(s: Size): TableState =
      s.width match {
        case w if w < PhoneCut =>
          copy(columns = columns.map {
            case c @ ColumnMeta(ObsNameColumn, _, _)    => c.copy(visible = false)
            case c @ ColumnMeta(TargetNameColumn, _, _) => c.copy(visible = false)
            case c                                      => c
          })
        case w if w < LargePhoneCut =>
          copy(columns = columns.map {
            case c @ ColumnMeta(TargetNameColumn, _, _) => c.copy(visible = false)
            case c                                      => c
          })
        case _ => this
      }
  }

  object TableState {
    val Zero: TableState = TableState(false, false, ColumnMeta.all)
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
              runningStep: Option[RunningStep]): QueueRow = {
      val p = (new js.Object).asInstanceOf[QueueRow]
      p.obsId = obsId
      p.status = status
      p.instrument = instrument
      p.targetName = targetName
      p.name = name
      p.active = active
      p.runningStep = runningStep
      p
    }

    def unapply(l: QueueRow): Option[(Observation.Id,
                                      SequenceState,
                                      Instrument,
                                      Option[String],
                                      String,
                                      Boolean,
                                      Option[RunningStep])] =
      Some((l.obsId, l.status, l.instrument, l.targetName, l.name, l.active, l.runningStep))

    val Zero: QueueRow =
      apply(Observation.Id.unsafeFromString("0"), SequenceState.Idle, Instrument.F2, None, "", active = false, None)
  }

  private def showSequence(p: Props, i: Instrument, id: Observation.Id): Callback =
    // Request to display the selected sequence
    p.sequences.dispatchCB(NavigateTo(SequencePage(i, id, 0)))

  private def linkTo(p: Props, page: SequencePage)(mod: TagMod*) =
    <.a(
      ^.href := p.ctl.urlFor(page).value,
      ^.onClick --> showSequence(p, page.instrument, page.obsId),
      p.ctl.setOnLinkClick(page),
      mod.toTagMod
    )

  private def linkedTextRenderer(p: Props)(
      f: QueueRow => TagMod): CellRenderer[js.Object, js.Object, QueueRow] =
    (_, _, _, row: QueueRow, _) => {
      val page = SequencePage(row.instrument, row.obsId, 0)
      linkTo(p, page)(SeqexecStyles.queueTextColumn, f(row))
    }

  private def obsIdRenderer(p: Props) = linkedTextRenderer(p) { r =>
    <.p(SeqexecStyles.queueText, r.obsId.format)
  }

  private def obsNameRenderer(p: Props) = linkedTextRenderer(p) { r =>
    <.p(SeqexecStyles.queueText, r.name)
  }

  private def statusText(status: SequenceState, runningStep: Option[RunningStep]): String =
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
    val targetName = r.targetName.fold(daytimeCalibrationTargetName)(x => x: TagMod)
    <.p(SeqexecStyles.queueText, targetName)
  }

  private def statusIconRenderer(p: Props): CellRenderer[js.Object, js.Object, QueueRow] =
    (_, _, _, row: QueueRow, _) => {
      val icon: TagMod =
        row.status match {
          case SequenceState.Completed =>
            IconCheckmark.copyIcon(fitted = true, extraStyles = List(SeqexecStyles.selectedIcon))
          case SequenceState.Running(_, _) =>
            IconCircleNotched.copyIcon(fitted = true,
                                       loading = true,
                                       extraStyles = List(SeqexecStyles.runningIcon))
          case SequenceState.Failed(_) =>
            IconAttention.copyIcon(fitted = true, extraStyles = List(SeqexecStyles.selectedIcon))
          case _ =>
            if (row.active)
              IconSelectedRadio.copyIcon(fitted = true,
                                         extraStyles = List(SeqexecStyles.selectedIcon))
            else iconEmpty
        }

      val page = SequencePage(row.instrument, row.obsId, 0)
      linkTo(p, page)(
        SeqexecStyles.queueIconColumn,
        icon
      )
    }

  private val statusHeaderRenderer: HeaderRenderer[js.Object] = (_, _, _, _, _, _) =>
    <.div(
      ^.title := "Control",
      ^.width := IconColumnWidth.px
  )

  // Renderer for a resizable column
  private def resizableHeaderRenderer(
      rs: (String, JsNumber) => Callback): HeaderRenderer[js.Object] =
    (_, dataKey: String, _, label: VdomNode, _, _) =>
      ReactFragment.withKey(dataKey)(
        <.div(
          ^.cls := "ReactVirtualized__Table__headerTruncatedText",
          label
        ),
        Draggable(
          Draggable.props(
            axis = Axis.X,
            defaultClassName = "DragHandle",
            defaultClassNameDragging = "DragHandleActive",
            onDrag = (ev: MouseEvent, d: DraggableData) => rs(dataKey, d.deltaX),
            position = ControlPosition(0)
          ),
          <.span(^.cls := "DragHandleIcon", "⋮")
        )
    )

  def rowClassName(p: Props)(i: Int): String =
    ((i, p.rowGetter(i)) match {
      case (-1, _) =>
        SeqexecStyles.headerRowStyle
      case (_, QueueRow(_, s, _, _, _, _, _)) if s == SequenceState.Completed  =>
        SeqexecStyles.stepRow |+| SeqexecStyles.rowPositive
      case (_, QueueRow(_, s, _, _, _, _, _)) if s.isRunning                   =>
        SeqexecStyles.stepRow |+| SeqexecStyles.rowWarning
      case (_, QueueRow(_, s, _, _, _, _, _)) if s.isError                     =>
        SeqexecStyles.stepRow |+| SeqexecStyles.rowNegative
      case (_, QueueRow(_, s, _, _, _, active, _)) if active && !s.isInProcess =>
        SeqexecStyles.stepRow |+| SeqexecStyles.rowActive
      case _                                                                   =>
        SeqexecStyles.stepRow
    }).htmlClass

  // scalastyle:off
  private def columns($ : $, size: Size): List[Table.ColumnArg] = {
    val props = $.props

    // Tell the model to resize a column
    def resizeRow(c: TableColumn): (String, JsNumber) => Callback =
      (_, dx) =>
        $.modState { s =>
          val percentDelta = dx.toDouble / size.width
          s.applyOffset(c, percentDelta)
      }

    $.state.visibleColumnsSizes(size).collect {
      case (IconColumn, width, _) =>
        Column(
          Column.props(
            width,
            dataKey = "status",
            flexShrink = 0,
            flexGrow = 0,
            label = "",
            cellRenderer = statusIconRenderer(props),
            headerRenderer = statusHeaderRenderer,
            className = SeqexecStyles.queueIconColumn.htmlClass
          ))
      case (ObsIdColumn, width, _) =>
        Column(
          Column.props(
            width,
            dataKey = "obsid",
            flexShrink = 0,
            flexGrow = 0,
            minWidth = ObsIdColumnWidth,
            label = "Obs. ID",
            cellRenderer = obsIdRenderer(props),
            headerRenderer = resizableHeaderRenderer(resizeRow(ObsIdColumn)),
            className = QueueColumnStyle
          ))
      case (StateColumn, width, _) =>
        Column(
          Column.props(
            width,
            dataKey = "state",
            minWidth = StateColumnWidth,
            flexShrink = 0,
            flexGrow = 0,
            label = "State",
            cellRenderer = stateRenderer(props),
            headerRenderer = resizableHeaderRenderer(resizeRow(StateColumn)),
            className = QueueColumnStyle
          ))
      case (InstrumentColumn, width, false) =>
        Column(
          Column.props(
            width,
            dataKey = "instrument",
            minWidth = InstrumentColumnWidth,
            flexShrink = 0,
            flexGrow = 0,
            label = "Instrument",
            cellRenderer = instrumentRenderer(props),
            headerRenderer = resizableHeaderRenderer(resizeRow(InstrumentColumn)),
            className = QueueColumnStyle
          ))
      case (InstrumentColumn, width, true) =>
        Column(
          Column.props(
            width,
            dataKey = "instrument",
            minWidth = InstrumentColumnWidth,
            flexShrink = 0,
            flexGrow = 0,
            label = "Instrument",
            cellRenderer = instrumentRenderer(props),
            className = QueueColumnStyle
          ))
      case (ObsNameColumn, width, _) =>
        Column(
          Column.props(
            width,
            dataKey = "obsName",
            minWidth = ObsNameColumnWidth / 2,
            flexShrink = 0,
            flexGrow = 0,
            label = "Obs. Name",
            cellRenderer = obsNameRenderer(props),
            className = QueueColumnStyle
          ))
      case (TargetNameColumn, width, _) =>
        Column(
          Column.props(
            width,
            dataKey = "target",
            minWidth = TargetNameColumnWidth / 2,
            flexShrink = 0,
            flexGrow = 0,
            label = "Target",
            cellRenderer = targetRenderer(props),
            headerRenderer = resizableHeaderRenderer(resizeRow(TargetNameColumn)),
            className = QueueColumnStyle
          ))
    }
  }
  // scalastyle:on

  def table(rs: $)(size: Size): VdomNode =
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
        rowCount = rs.props.rowCount,
        rowHeight = SeqexecStyles.rowHeight,
        rowClassName = rowClassName(rs.props) _,
        width = size.width.toInt,
        rowGetter = rs.props.rowGetter _,
        headerClassName = SeqexecStyles.tableHeader.htmlClass,
        headerHeight = SeqexecStyles.headerHeight
      ),
      columns(rs, size): _*
    ).vdomElement

  def initialState(p: Props): TableState =
    TableState.Zero.loginState(p.isLogged).withWidths(p.sequencesList)

  private val component = ScalaComponent
    .builder[Props]("QueueTableBody")
    .initialStateFromProps(initialState)
    .renderPS(($, _, _) => AutoSizer(AutoSizer.props(table($), disableHeight = true)))
    .componentWillReceiveProps { $ =>
      $.modState { s =>
        s.loginState($.nextProps.isLogged).withWidths($.nextProps.sequencesList)
      }
    }
    .build

  def apply(ctl: RouterCtl[SeqexecPages],
            p: ModelProxy[StatusAndLoadedSequencesFocus]): Unmounted[Props, TableState, Unit] =
    component(Props(ctl, p))

}
