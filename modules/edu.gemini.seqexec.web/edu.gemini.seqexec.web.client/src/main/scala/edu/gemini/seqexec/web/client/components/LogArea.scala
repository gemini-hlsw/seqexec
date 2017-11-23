// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components

import scala.scalajs.js
import diode.react.ModelProxy
import edu.gemini.seqexec.model.Model.ServerLogLevel
import edu.gemini.seqexec.web.client.model.GlobalLog
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.Unmounted
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
  implicit val showLevel: Show[ServerLogLevel] = Show.showFromToString
  implicit val showInstant: Show[Instant] = Show.showFromToString
  // ScalaJS defined trait
  trait LogRow extends js.Object {
    var timestamp: String
    var level: String
    var msg: String
  }
  object LogRow {
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def apply(timestamp: String, level: String, msg: String): LogRow = {
      val p = (new js.Object).asInstanceOf[LogRow]
      p.timestamp = timestamp
      p.level = level
      p.msg = msg
      p
    }
    val Zero: LogRow = apply("", "", "")
  }
  final case class Props(log: ModelProxy[GlobalLog]) {
    def rowGetter(i: Int): LogRow = log().log.index(i).map { l =>
      LogRow(l.timestamp.shows, l.level.shows, l.msg)
    }.getOrElse(LogRow.Zero)
  }

  def table(p: Props)(size: Size): VdomNode = {
    val columns = List(
      Column(Column.props(200, "timestamp", label = "Timestamp", disableSort = true)),
      Column(Column.props(80, "level", label = "Level", disableSort = true)),
      Column(Column.props(size.width.toInt - 200 - 80, "msg", label = "Message", disableSort = true, flexGrow = 1))
    )

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
        rowCount = p.log().log.size,
        rowHeight = 30,
        width = size.width.toInt,
        rowGetter = p.rowGetter _,
        headerClassName = SeqexecStyles.logTableHeader.htmlClass,
        headerHeight = 47),
      columns: _*).vdomElement
  }

  private val component = ScalaComponent.builder[Props]("LogArea")
    .stateless
    .render_P { p =>
      <.div(
        ^.cls := "ui sixteen wide column",
        <.div(
          ^.cls := "ui secondary segment",
          <.div(
            ^.cls := "ui form",
            <.div(
              ^.cls := "field",
              AutoSizer(AutoSizer.props(table(p), disableHeight = true))
            )
          )
        )
      )
    }
    .build

  def apply(p: ModelProxy[GlobalLog]): Unmounted[Props, Unit, Unit] = component(Props(p))
}
