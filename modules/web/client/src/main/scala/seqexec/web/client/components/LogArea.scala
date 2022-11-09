// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import java.time.Instant
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.collection.immutable.SortedMap
import scala.math.max
import scala.scalajs.js

import cats._
import cats.data.NonEmptyList
import cats.syntax.all._
import cats.Order._
import japgolly.scalajs.react.ReactMonocle._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enums.Site
import monocle.Lens
import monocle.function.At.at
import monocle.function.At.atSortedMap
import monocle.macros.Lenses
import mouse.all._
import react.clipboard._
import react.common._
import react.common.implicits._
import react.semanticui.collections.form.Form
import react.semanticui.collections.form.FormCheckbox
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.LabelPosition
import react.semanticui.elements.segment.Segment
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.sizes._
import react.semanticui.textalignment._
import react.virtualized._
import seqexec.common.FixedLengthBuffer
import seqexec.model.enum.ServerLogLevel
import seqexec.model.events._
import seqexec.web.client.actions.ToggleLogArea
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.icons._
import seqexec.web.client.model.GlobalLog
import seqexec.web.client.model.SectionVisibilityState.SectionOpen
import seqexec.web.client.reusability._
import web.client.table._

/**
 * Area to display a sequence's log
 */
object CopyLogToClipboard {
  private val component = ScalaComponent
    .builder[String]
    .stateless
    .render_P { p =>
      CopyToClipboard(p)(
        <.div(IconCopyOutline.link().clazz(SeqexecStyles.logIconRow))
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: String): Unmounted[String, Unit, Unit] = component(p)
}

/**
 * Area to display a sequence's log
 */
object LogArea {
  type Backend = RenderScope[Props, State, Unit]

  implicit val show: Show[ServerLogLevel] =
    Show.show(_.label)

  sealed trait TableColumn
  case object TimestampColumn extends TableColumn
  case object LevelColumn     extends TableColumn
  case object MsgColumn       extends TableColumn
  case object ClipboardColumn extends TableColumn

  object TableColumn {
    implicit val eq: Eq[TableColumn]               = Eq.fromUniversalEquals
    implicit val tcReuse: Reusability[TableColumn] = Reusability.byRef
  }

  // Date time formatter
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SS")

  // ScalaJS defined trait
  trait LogRow extends js.Object {
    var local: String // Formatted string
    var timestamp: Instant
    var level: ServerLogLevel
    var msg: String
    var clip: String
  }

  object LogRow {

    def apply(local: String, timestamp: Instant, level: ServerLogLevel, msg: String): LogRow = {
      val p = (new js.Object).asInstanceOf[LogRow]
      p.local = local
      p.timestamp = timestamp
      p.level = level
      p.msg = msg
      p.clip = ""
      p
    }

    def unapply(l: LogRow): Option[(Instant, ServerLogLevel, String, String)] =
      Some((l.timestamp, l.level, l.msg, l.clip))

    val Default: LogRow = apply("", Instant.MAX, ServerLogLevel.INFO, "")
  }

  final case class Props(site: Site, log: GlobalLog) {
    val reverseLog: FixedLengthBuffer[ServerLogMessage] = log.log.reverse

    // Filter according to the levels on the controls
    private def levelFilter(s: State)(m: ServerLogMessage): Boolean =
      s.allowedLevel(m.level)

    def rowGetter(s: State)(i: Int): LogRow =
      reverseLog
        .filter_(levelFilter(s) _)
        .lift(i)
        .fold(LogRow.Default) { l =>
          val localTime = LocalDateTime.ofInstant(l.timestamp, site.timezone)
          LogRow(formatter.format(localTime), l.timestamp, l.level, l.msg)
        }

    def rowCount(s: State): Int =
      reverseLog.filter_(levelFilter(s) _).size

  }

  @Lenses
  final case class State(
    selectedLevels: SortedMap[ServerLogLevel, Boolean],
    tableState:     TableState[TableColumn]
  ) {

    def allowedLevel(level: ServerLogLevel): Boolean =
      selectedLevels.getOrElse(level, false)

  }

  implicit val propsReuse: Reusability[Props] =
    Reusability.derive[Props]
  implicit val stateReuse: Reusability[State] =
    Reusability.by(x => (x.selectedLevels.toList, x.tableState))

  object State {

    def levelLens(l: ServerLogLevel): Lens[State, Option[Boolean]] =
      State.selectedLevels.andThen(at(l))

    private val DefaultTableState: TableState[TableColumn] =
      TableState[TableColumn](
        userModified = NotModified,
        scrollPosition = 0,
        columns =
          NonEmptyList.of(TimestampColumnMeta, LevelColumnMeta, MsgColumnMeta, ClipboardColumnMeta)
      )

    val Default: State =
      State(SortedMap(ServerLogLevel.ServerLogLevelEnumerated.all.map(_ -> true): _*),
            DefaultTableState
      )
  }

  private val ClipboardWidth    = 41.0
  private val TimestampMinWidth = 90.1667 + SeqexecStyles.TableBorderWidth
  private val LevelMinWidth     = 59.3333 + SeqexecStyles.TableBorderWidth
  private val MessageMinWidth   = 89.35 + SeqexecStyles.TableBorderWidth

  private val TimestampColumnMeta: ColumnMeta[TableColumn] =
    ColumnMeta[TableColumn](
      column = TimestampColumn,
      name = "local",
      label = "Timestamp",
      visible = true,
      width = VariableColumnWidth.unsafeFromDouble(0.2, TimestampMinWidth)
    )

  private val LevelColumnMeta: ColumnMeta[TableColumn] =
    ColumnMeta[TableColumn](
      column = LevelColumn,
      name = "level",
      label = "Level",
      visible = true,
      grow = 1,
      removeable = 2,
      width = VariableColumnWidth.unsafeFromDouble(0.1, LevelMinWidth)
    )

  private val MsgColumnMeta: ColumnMeta[TableColumn] =
    ColumnMeta[TableColumn](
      column = MsgColumn,
      name = "msg",
      label = "Message",
      visible = true,
      grow = 10,
      width = VariableColumnWidth.unsafeFromDouble(0.7, MessageMinWidth)
    )

  private val ClipboardColumnMeta: ColumnMeta[TableColumn] =
    ColumnMeta[TableColumn](
      column = ClipboardColumn,
      name = "clip",
      label = "",
      visible = true,
      width = FixedColumnWidth.unsafeFromDouble(ClipboardWidth)
    )

  private val columnWidths: TableColumn => Option[Double] = {
    case TimestampColumn => 200.0.some
    case LevelColumn     => 100.0.some
    case MsgColumn       => 400.0.some
    case _               => none
  }

  private val LogColumnStyle: String = SeqexecStyles.queueText.htmlClass

  // Custom renderers for the last column
  private val clipboardHeaderRenderer: HeaderRenderer[js.Object] =
    (_, _, _, _, _, _) => IconCopyOutline.clazz(SeqexecStyles.logIconHeader)

  private def colBuilder(b: Backend, size: Size)(
    r:                      ColumnRenderArgs[TableColumn]
  ): Table.ColumnArg =
    r match {
      case ColumnRenderArgs(meta, _, _, _) if meta.column === ClipboardColumn =>
        Column(
          Column.propsNoFlex(
            width = ClipboardWidth,
            dataKey = meta.name,
            headerRenderer = clipboardHeaderRenderer,
            cellRenderer = clipboardCellRenderer(b.props.site),
            className = SeqexecStyles.clipboardIconDiv.htmlClass,
            headerClassName = SeqexecStyles.clipboardIconHeader.htmlClass
          )
        )
      case ColumnRenderArgs(meta, _, width, _) if meta.column === MsgColumn   =>
        Column(
          Column.propsNoFlex(width = width,
                             dataKey = meta.name,
                             label = meta.label,
                             className = LogColumnStyle
          )
        )
      case ColumnRenderArgs(meta, _, width, _)                                =>
        Column(
          Column.propsNoFlex(
            width = width,
            dataKey = meta.name,
            label = meta.label,
            headerRenderer = resizableHeaderRenderer(
              b.state.tableState
                .resizeColumn(meta.column, size, b.setStateL(State.tableState)(_))
            ),
            className = LogColumnStyle
          )
        )
    }

  private def clipboardCellRenderer(
    site: Site
  ): CellRenderer[js.Object, js.Object, LogRow] =
    (_, _, _, row: LogRow, _) => {
      // Simple csv export
      val localTime = LocalDateTime.ofInstant(row.timestamp, site.timezone)
      val toCsv     = s"${formatter.format(localTime)}, ${row.level}, ${row.msg}"
      CopyLogToClipboard(toCsv)
    }

  // Style for each row
  private def rowClassName(b: Backend)(i: Int): String =
    ((i, b.props.rowGetter(b.state)(i)) match {
      case (-1, _)                                    =>
        SeqexecStyles.headerRowStyle
      case (_, LogRow(_, ServerLogLevel.INFO, _, _))  =>
        SeqexecStyles.stepRow |+| SeqexecStyles.infoLog
      case (_, LogRow(_, ServerLogLevel.WARN, _, _))  =>
        SeqexecStyles.stepRow |+| SeqexecStyles.warningLog
      case (_, LogRow(_, ServerLogLevel.ERROR, _, _)) =>
        SeqexecStyles.stepRow |+| SeqexecStyles.errorLog
      case _                                          =>
        SeqexecStyles.stepRow
    }).htmlClass

  /**
   * Build the table log
   */
  private def table(b: Backend)(size: Size): VdomNode =
    if (size.width.toInt > 0)
      Table(
        Table.props(
          disableHeader = false,
          noRowsRenderer = () =>
            Segment(textAlign = Center, clazz = SeqexecStyles.noRowsSegment)(
              ^.height := 270.px,
              "No log entries"
            ),
          overscanRowCount = SeqexecStyles.overscanRowCount,
          height = 200,
          rowCount = b.props.rowCount(b.state),
          rowHeight = SeqexecStyles.rowHeight,
          rowClassName = rowClassName(b) _,
          width = max(1, size.width.toInt),
          rowGetter = b.props.rowGetter(b.state) _,
          headerClassName = SeqexecStyles.tableHeader.htmlClass,
          headerHeight = SeqexecStyles.headerHeight
        ),
        b.state.tableState.columnBuilder(size, colBuilder(b, size)): _*
      ).vdomElement
    else
      <.div()

  private def onResize(b: Backend): Size => Callback =
    s => b.modStateL(State.tableState)(_.recalculateWidths(s, _ => true, columnWidths))

  private def onLevelChange(
    b: Backend,
    l: ServerLogLevel
  ): Boolean => Callback =
    s => b.setStateL(State.levelLens(l))(s.some)

  private val component = ScalaComponent
    .builder[Props]
    .initialState(State.Default)
    .render { b =>
      val p          = b.props
      val s          = b.state
      val toggleIcon =
        if (p.log.display === SectionOpen)
          IconDoubleDown
        else IconDoubleUp
      val toggleText =
        (p.log.display === SectionOpen).fold("Hide Log", "Show Log")
      Segment(secondary = true, clazz = SeqexecStyles.logSecondarySegment)(
        <.div(SeqexecStyles.logControlRow)(
          Button(icon = true,
                 labelPosition = LabelPosition.Left,
                 compact = true,
                 size = Small,
                 onClick = SeqexecCircuit.dispatchCB(ToggleLogArea)
          )(toggleIcon, toggleText),
          Form(
            <.div(SeqexecStyles.selectorFields)(
              s.selectedLevels.toTagMod { case (l, s) =>
                FormCheckbox(
                  label = l.show,
                  inline = true,
                  clazz = SeqexecStyles.logLevelBox,
                  checked = s,
                  onChangeE = (_: ReactMouseEvent, p: Checkbox.CheckboxProps) =>
                    onLevelChange(b, l)(p.checked.getOrElse(false))
                )
              }
            )
          ).when(p.log.display === SectionOpen)
        ),
        <.div(SeqexecStyles.logSecondarySegment |+| SeqexecStyles.logTable)(
          AutoSizer(AutoSizer.props(table(b), disableHeight = true, onResize = onResize(b)))
        ).when(p.log.display === SectionOpen)
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(site: Site, p: GlobalLog): Unmounted[Props, State, Unit] =
    component(Props(site, p))
}
