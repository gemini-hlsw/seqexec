// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import cats.Eq
import cats.data.NonEmptyList
import cats.implicits._
import mouse.all._
import diode.react.ModelProxy
import gem.enum.Site
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.vdom.html_<^._
import java.time.{Instant, LocalDateTime}
import java.time.format.DateTimeFormatter
import react.virtualized._
import react.clipboard._
import scala.scalajs.js
import seqexec.model.Model.ServerLogLevel
import seqexec.model.events._
import seqexec.web.client.semanticui.elements.checkbox.Checkbox
import seqexec.web.client.semanticui.elements.icon.Icon.{IconCopy, IconAngleDoubleDown, IconAngleDoubleUp}
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.button.Button.LeftLabeled
import seqexec.web.client.semanticui.{Size => SSize}
import seqexec.web.client.model.{GlobalLog, SectionOpen}
import seqexec.web.client.actions.ToggleLogArea
import seqexec.web.common.FixedLengthBuffer
import web.client.style._
import web.client.table._

/**
  * Area to display a sequence's log
  */
object CopyLogToClipboard {
  private val component = ScalaComponent
    .builder[String]("CopyLogToClipboard")
    .stateless
    .render_P { p =>
      // Callback
      val onCopy: OnCopy = (_, _) => Callback.log(s"Copied $p")
      CopyToClipboard(
        CopyToClipboard.props(p, onCopy = onCopy),
        <.div(
          IconCopy.copyIcon(link = true,
                            extraStyles = List(SeqexecStyles.logIconRow))))
    }
    .build

  def apply(p: String): Unmounted[String, Unit, Unit] = component(p)
}

/**
  * Area to display a sequence's log
  */
object LogArea {
  type Backend = RenderScope[Props, State, Unit]

  sealed trait TableColumn
  case object TimestampColumn extends TableColumn
  case object LevelColumn     extends TableColumn
  case object MsgColumn       extends TableColumn
  case object ClipboardColumn extends TableColumn

  object TableColumn {
    implicit val eq: Eq[TableColumn] = Eq.fromUniversalEquals
  }

  // Date time formatter
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SS")

  // ScalaJS defined trait
  // scalastyle:off
  trait LogRow extends js.Object {
    var local    : String // Formatted string
    var timestamp: Instant
    var level    : ServerLogLevel
    var msg      : String
    var clip     : String
  }

  // scalastyle:on
  object LogRow {

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def apply(local    : String,
              timestamp: Instant,
              level    : ServerLogLevel,
              msg      : String): LogRow = {
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

    val Zero: LogRow = apply("", Instant.MAX, ServerLogLevel.INFO, "")
  }

  final case class Props(site: Site, log: ModelProxy[GlobalLog]) {
    val reverseLog: FixedLengthBuffer[ServerLogMessage] = log().log.reverse

    // Filter according to the levels on the controls
    private def levelFilter(s: State)(m: ServerLogMessage): Boolean =
      s.allowedLevel(m.level)

    def rowGetter(s: State)(i: Int): LogRow =
      reverseLog
        .filter_(levelFilter(s) _)
        .lift(i)
        .map { l =>
          val localTime = LocalDateTime.ofInstant(l.timestamp, site.timezone)
          LogRow(formatter.format(localTime), l.timestamp, l.level, l.msg)
        }
        .getOrElse(LogRow.Zero)

    def rowCount(s: State): Int =
      reverseLog.filter_(levelFilter(s) _).size

  }

  final case class State(selectedLevels: Map[ServerLogLevel, Boolean],
                         tableState: TableState[TableColumn]) {

    def updateLevel(level: ServerLogLevel, value: Boolean): State =
      copy(selectedLevels + (level -> value))

    def allowedLevel(level: ServerLogLevel): Boolean =
      selectedLevels.getOrElse(level, false)
  }

  object State {

    val ZeroTableState: TableState[TableColumn] = TableState[TableColumn](
      userModified   = NotModified,
      scrollPosition = 0,
      columns        = NonEmptyList.of(TimestampColumnMeta,
                                       LevelColumnMeta,
                                       MsgColumnMeta,
                                       ClipboardColumnMeta))

    val Zero: State =
      State(ServerLogLevel.all.map(_ -> true).toMap, ZeroTableState)
  }

  private val ST = ReactS.Fix[State]

  private val ClipboardWidth = 37

  val TimestampColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    column  = TimestampColumn,
    name    = "local",
    label   = "Timestamp",
    visible = true,
    width   = PercentageColumnWidth.unsafeFromDouble(0.2)
  )

  val LevelColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    column  = LevelColumn,
    name    = "level",
    label   = "Level",
    visible = true,
    width   = PercentageColumnWidth.unsafeFromDouble(0.1)
  )

  val MsgColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    column  = MsgColumn,
    name    = "msg",
    label   = "Message",
    visible = true,
    width   = PercentageColumnWidth.unsafeFromDouble(0.7)
  )

  val ClipboardColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    column  = ClipboardColumn,
    name    = "clip",
    label   = "",
    visible = true,
    width   = FixedColumnWidth(ClipboardWidth)
  )

  private val LogColumnStyle: String = SeqexecStyles.queueText.htmlClass

  private def updateTableState(st: TableState[TableColumn]) =
    ST.mod(_.copy(tableState = st)).liftCB

  /**
    * Build the table log
    */
  def table(b: Backend)(size: Size): VdomNode = {
    val p = b.props
    val s = b.state

    // Custom renderers for the last column
    val clipboardHeaderRenderer: HeaderRenderer[js.Object] =
      (_, _, _, _, _, _) =>
        IconCopy.copyIcon(extraStyles = List(SeqexecStyles.logIconHeader))

    val clipboardCellRenderer: CellRenderer[js.Object, js.Object, LogRow] =
      (_, _, _, row: LogRow, _) => {
        // Simple csv export
        val localTime = LocalDateTime.ofInstant(row.timestamp, p.site.timezone)
        val toCsv     = s"${formatter.format(localTime)}, ${row.level}, ${row.msg}"
        CopyLogToClipboard(toCsv)
      }

    def colBuilder(r: ColumnRenderArgs[TableColumn]): Table.ColumnArg =
      r match {
        case ColumnRenderArgs(ColumnMeta(ClipboardColumn, name, _, _, _), _, _, _) =>
          Column(
            Column.propsNoFlex(
              width           = ClipboardWidth,
              dataKey         = name,
              headerRenderer  = clipboardHeaderRenderer,
              cellRenderer    = clipboardCellRenderer,
              className       = SeqexecStyles.clipboardIconDiv.htmlClass,
              headerClassName = SeqexecStyles.clipboardIconHeader.htmlClass
            ))
        case ColumnRenderArgs(ColumnMeta(MsgColumn, name, label, _, _), _, width, _) =>
          Column(
            Column.propsNoFlex(
              width     = width,
              dataKey   = name,
              label     = label,
              className = LogColumnStyle))
        case ColumnRenderArgs(ColumnMeta(c, name, label, _, _), _, width, _) =>
          Column(
            Column.propsNoFlex(
              width          = width,
              dataKey        = name,
              label          = label,
              headerRenderer = resizableHeaderRenderer(s.tableState
                .resizeRow(c, size, x => b.runState(updateTableState(x)))),
              className      = LogColumnStyle
            ))
      }

    def rowClassName(s: State)(i: Int): String =
      ((i, p.rowGetter(s)(i)) match {
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

    Table(
      Table.props(
        disableHeader = false,
        noRowsRenderer = () =>
          <.div(
            ^.cls := "ui center aligned segment noRows",
            SeqexecStyles.noRowsSegment,
            ^.height := 270.px,
            "No log entries"
        ),
        overscanRowCount = SeqexecStyles.overscanRowCount,
        height = 200,
        rowCount = p.rowCount(s),
        rowHeight = SeqexecStyles.rowHeight,
        rowClassName = rowClassName(s) _,
        width = size.width,
        rowGetter = p.rowGetter(s) _,
        headerClassName = SeqexecStyles.tableHeader.htmlClass,
        headerHeight = SeqexecStyles.headerHeight
      ),
      s.tableState.columnBuilder(size, colBuilder): _*
    ).vdomElement
  }

  private def updateState(level: ServerLogLevel)(value: Boolean) =
    ST.mod(_.updateLevel(level, value)).liftCB

  private val component = ScalaComponent
    .builder[Props]("LogArea")
    .initialState(State.Zero)
    .renderPS { ($, p, s) =>
      val toggleIcon = (p.log().display === SectionOpen)
        .fold(IconAngleDoubleDown, IconAngleDoubleUp)
      val toggleText =
        (p.log().display === SectionOpen).fold("Hide Log", "Show Log")
      <.div(
        ^.cls := "ui sixteen wide column",
        SeqexecStyles.logSegment,
        <.div(
          ^.cls := "ui secondary segment",
          SeqexecStyles.logSecondarySegment,
          <.div(
            ^.cls := "ui grid",
            <.div(
              ^.cls := "ui row",
              SeqexecStyles.logControlRow,
              <.div(
                ^.cls := "ui six wide column",
                Button(Button.Props(icon    = Option(toggleIcon),
                                    labeled = LeftLabeled,
                                    compact = true,
                                    size    = SSize.Small,
                                    onClick = p.log.dispatchCB(ToggleLogArea)),
                       toggleText)
              ),
              <.div(
                ^.cls := "ui ten wide column",
                <.div(
                  ^.cls := "ui form row",
                  <.div(
                    ^.cls := "fields",
                    SeqexecStyles.selectorFields,
                    s.selectedLevels.toTagMod {
                      case (l, s) =>
                        <.div(
                          ^.cls := "inline field",
                          Checkbox(
                            Checkbox.Props(l.show,
                                           s,
                                           v => $.runState(updateState(l)(v))))
                        )
                    }
                  )
                )
              ).when(p.log().display === SectionOpen)
            ),
            <.div(
              ^.cls := "ui row",
              SeqexecStyles.logTableRow,
              AutoSizer(AutoSizer.props(table($), disableHeight = true))
            ).when(p.log().display === SectionOpen)
          )
        )
      )
    }
    .build

  def apply(site: Site,
            p: ModelProxy[GlobalLog]): Unmounted[Props, State, Unit] =
    component(Props(site, p))
}
