// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components

import scala.scalajs.js
import diode.react.ModelProxy
import edu.gemini.seqexec.model.Model.ServerLogLevel
import edu.gemini.seqexec.model.Model.SeqexecEvent.ServerLogMessage
import edu.gemini.seqexec.web.client.semanticui.elements.slider.Slider
import edu.gemini.seqexec.web.client.model.GlobalLog
import edu.gemini.web.common.FixedLengthBuffer
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.vdom.html_<^._
import react.virtualized._
import java.time.Instant

import scalaz.Show
import scalaz.syntax.foldable._
import scalaz.syntax.show._

/**
  * Area to display a sequence's log
  */
object LogArea {
  implicit val showInstant: Show[Instant] = Show.showFromToString
  // ScalaJS defined trait
  // scalastyle:off
  trait LogRow extends js.Object {
    var timestamp: String
    var level: ServerLogLevel
    var msg: String
  }
  // scalastyle:on
  object LogRow {
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def apply(timestamp: String, level: ServerLogLevel, msg: String): LogRow = {
      val p = (new js.Object).asInstanceOf[LogRow]
      p.timestamp = timestamp
      p.level = level
      p.msg = msg
      p
    }

    def unapply(l: LogRow): Option[(String, ServerLogLevel, String)] =
      Some((l.timestamp, l.level, l.msg))

    val Zero: LogRow = apply("", ServerLogLevel.INFO, "")
  }
  final case class Props(log: ModelProxy[GlobalLog]) {
    val reverseLog: FixedLengthBuffer[ServerLogMessage] = log().log.reverse
    def rowGetter(i: Int): LogRow = reverseLog.index(i).map { l =>
      LogRow(l.timestamp.shows, l.level, l.msg)
    }.getOrElse(LogRow.Zero)
  }
  final case class State(infoSelected: Boolean)

  private val ST = ReactS.Fix[State]

  /**
   * Build the table log
   */
  def table(p: Props)(size: Size): VdomNode = {
    val rowCount = p.log().log.size

    val columns = List(
      Column(Column.props(200, "timestamp", label = "Timestamp", disableSort = true)),
      Column(Column.props(80, "level", label = "Level", disableSort = true)),
      Column(Column.props(size.width.toInt - 200 - 80, "msg", label = "Message", disableSort = true, flexGrow = 1))
    )

    def rowClassName(i: Int): String = p.reverseLog.index(i).map {
      case ServerLogMessage(ServerLogLevel.INFO, _, _)  => SeqexecStyles.infoLog
      case ServerLogMessage(ServerLogLevel.WARN, _, _)  => SeqexecStyles.warningLog
      case ServerLogMessage(ServerLogLevel.ERROR, _, _) => SeqexecStyles.errorLog
    }.map(_.htmlClass).getOrElse(SeqexecStyles.logRow.htmlClass)

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
        rowCount = rowCount,
        rowHeight = 30,
        rowClassName = rowClassName _,
        width = size.width.toInt,
        rowGetter = p.rowGetter _,
        headerClassName = SeqexecStyles.logTableHeader.htmlClass,
        headerHeight = 37),
      columns: _*).vdomElement
  }

  private def updateState(level: ServerLogLevel)(value: Boolean) =
    ST.set(State(value)).liftCB >> ST.retM(Callback.log(s"$level $value"))

  private val component = ScalaComponent.builder[Props]("LogArea")
    .initialState(State(true))
    .renderPS { ($, p, s) =>
      <.div(
        ^.cls := "ui sixteen wide column",
        <.div(
          ^.cls := "ui secondary segment",
          <.div(
            ^.cls := "ui form",
            <.div(
              ^.cls := "fields",
              <.div(
                ^.cls := "inline field",
                Slider(Slider.Props("INFO", s.infoSelected, v => $.runState(updateState(ServerLogLevel.INFO)(v))))
              ),
              /*<.div(
                ^.cls := "inline field",
                Slider(Slider.Props("WARN"))
              ),
              <.div(
                ^.cls := "inline field",
                Slider(Slider.Props("ERROR"))
              )*/
            ),
            <.div(
              ^.cls := "field",
              AutoSizer(AutoSizer.props(table(p), disableHeight = true))
            )
          )
        )
      )
    }
    .build

  def apply(p: ModelProxy[GlobalLog]): Unmounted[Props, State, Unit] = component(Props(p))
}
