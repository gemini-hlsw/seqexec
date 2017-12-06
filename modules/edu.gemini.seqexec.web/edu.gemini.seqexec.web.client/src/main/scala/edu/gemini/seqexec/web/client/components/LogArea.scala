// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components

import scala.scalajs.js
import diode.react.ModelProxy
import edu.gemini.seqexec.model.Model.{SeqexecSite, ServerLogLevel}
import edu.gemini.seqexec.model.events.SeqexecEvent.ServerLogMessage
import edu.gemini.seqexec.web.client.semanticui.elements.checkbox.Checkbox
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconCopy
import edu.gemini.seqexec.web.client.model.GlobalLog
import edu.gemini.web.common.FixedLengthBuffer
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.html_<^._
import react.virtualized._
import react.clipboard._
import java.time.{Instant, LocalDateTime}
import java.time.format.DateTimeFormatter
import scalacss.ScalaCssReact._

import scalaz.syntax.foldable._
import scalaz.syntax.monadPlus.{^ => _, _}
import scalaz.syntax.show._

/**
  * Area to display a sequence's log
  */
object CopyLogToClipboard {
  private val component = ScalaComponent.builder[String]("CopyLogToClipboard")
    .stateless
    .render_PS { (p, s) =>
      // Callback
      val onCopy: OnCopy = (_, _) => Callback.log(s"Copied $s")
      CopyToClipboard(CopyToClipboard.props(p, onCopy = onCopy), <.div(^.cls := "copydiv", IconCopy.copyIcon(link = true, extraStyles = List(SeqexecStyles.logIconRow))))
    }.build

  def apply(p: String): Unmounted[String, Unit, Unit] = component(p)
}
/**
  * Area to display a sequence's log
  */
object LogArea {
  // Date time formatter
  private val formatter  = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SS")
  // ScalaJS defined trait
  // scalastyle:off
  trait LogRow extends js.Object {
    var local: String // Formatted string
    var timestamp: Instant
    var level: ServerLogLevel
    var msg: String
    var clip: String
  }
  // scalastyle:on
  object LogRow {
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def apply(local: String, timestamp: Instant, level: ServerLogLevel, msg: String): LogRow = {
      val p = (new js.Object).asInstanceOf[LogRow]
      p.local =local
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

  final case class Props(site: SeqexecSite, log: ModelProxy[GlobalLog]) {
    val reverseLog: FixedLengthBuffer[ServerLogMessage] = log().log.reverse

    // Filter according to the levels on the controls
    private def levelFilter(s: State)(m: ServerLogMessage): Boolean = s.allowedLevel(m.level)

    def rowGetter(s: State)(i: Int): LogRow = reverseLog.filter(levelFilter(s) _).index(i).map { l =>
        val localTime = LocalDateTime.ofInstant(l.timestamp, site.timeZone)
        LogRow(formatter.format(localTime), l.timestamp, l.level, l.msg)
      }.getOrElse(LogRow.Zero)

    def rowCount(s: State): Int =
      reverseLog.filter(levelFilter(s) _).size

  }

  final case class State(selectedLevels: Map[ServerLogLevel, Boolean]) {
    def updateLevel(level: ServerLogLevel, value: Boolean): State = copy(selectedLevels + (level -> value))
    def allowedLevel(level: ServerLogLevel): Boolean = selectedLevels.getOrElse(level, false)
  }

  object State {
    val Zero: State = State(ServerLogLevel.all.map(_ -> true).toMap)
  }

  private val ST = ReactS.Fix[State]

  private val TimestampWidth = 200
  private val LevelWidth     = 80
  private val ClipboardWidth = 37

  /**
   * Build the table log
   */
  def table(p: Props, s: State)(size: Size): VdomNode = {

    // Custom renderers for the last column
    val clipboardHeaderRenderer: HeaderRenderer[js.Object] = (_, _, _, _, _, _) =>
      IconCopy.copyIcon(extraStyles = List(SeqexecStyles.logIconHeader))

    val clipboardCellRenderer: CellRenderer[js.Object, js.Object, LogRow] = (_, _, _, row: LogRow, _) => {
      // Simple csv export
      val localTime = LocalDateTime.ofInstant(row.timestamp, p.site.timeZone)
      val toCsv = s"${formatter.format(localTime)}, ${row.level}, ${row.msg}"
      CopyLogToClipboard(toCsv)
    }

    val columns = List(
      Column(Column.props(TimestampWidth, "local", label = "Timestamp", disableSort = true)),
      Column(Column.props(LevelWidth, "level", label = "Level", disableSort = true)),
      Column(Column.props(size.width.toInt - TimestampWidth - LevelWidth - ClipboardWidth, "msg", label = "Message", disableSort = true)),
      Column(Column.props(ClipboardWidth, "clip", disableSort = true, headerRenderer = clipboardHeaderRenderer, cellRenderer = clipboardCellRenderer))
    )

    def rowClassName(s: State)(i: Int): String = (p.rowGetter(s)(i) match {
      case LogRow(_, ServerLogLevel.INFO, _, _)  => SeqexecStyles.infoLog
      case LogRow(_, ServerLogLevel.WARN, _, _)  => SeqexecStyles.warningLog
      case LogRow(_, ServerLogLevel.ERROR, _, _) => SeqexecStyles.errorLog
      case _                                     => SeqexecStyles.logRow
    }).htmlClass

    Table(
      Table.props(
        disableHeader = false,
        noRowsRenderer = () =>
          <.div(
            ^.cls := "ui center aligned segment noRows",
            ^.height := 270.px,
            "No log entries"
          ),
        overscanRowCount = 10,
        height = 300,
        rowCount = p.rowCount(s),
        rowHeight = 30,
        rowClassName = rowClassName(s) _,
        width = size.width.toInt,
        rowGetter = p.rowGetter(s) _,
        headerClassName = SeqexecStyles.logTableHeader.htmlClass,
        headerHeight = 37),
      columns: _*).vdomElement
  }

  private def updateState(level: ServerLogLevel)(value: Boolean) =
    ST.mod(_.updateLevel(level, value)).liftCB

  private val component = ScalaComponent.builder[Props]("LogArea")
    .initialState(State.Zero)
    .renderPS { ($, p, s) =>
      <.div(
        ^.cls := "ui sixteen wide column",
        <.div(
          ^.cls := "ui secondary segment",
          <.div(
            ^.cls := "ui form",
            <.div(
              ^.cls := "fields",
              SeqexecStyles.selectorFields,
              s.selectedLevels.map {
                case (l, s) =>
                <.div(
                  ^.cls := "inline field",
                  Checkbox(Checkbox.Props(l.shows, s, v => $.runState(updateState(l)(v))))
                )
              }.toTagMod
            ),
            <.div(
              ^.cls := "field",
              AutoSizer(AutoSizer.props(table(p, s), disableHeight = true))
            )
          )
        )
      )
    }
    .build

  def apply(site: SeqexecSite, p: ModelProxy[GlobalLog]): Unmounted[Props, State, Unit] = component(Props(site, p))
}
