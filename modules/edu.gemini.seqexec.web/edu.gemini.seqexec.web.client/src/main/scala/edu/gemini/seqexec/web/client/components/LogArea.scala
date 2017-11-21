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

  private val columns = List(
    Column(Column.props(60, "timestamp", label = "Timestamp", disableSort = true)),
    Column(Column.props(90, "level", label = "Level", disableSort = true)),
    Column(Column.props(210, "msg", label = "Message", disableSort = true, flexGrow = 1))
  )

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
              <.label("Log"),
              Table(
                Table.props(
                  disableHeader = false,
                  noRowsRenderer = () => <.div(^.cls := "noRows", "No entries"),
                  overscanRowCount = 10,
                  height = 500,
                  rowCount = p.log().log.size,
                  rowHeight = 40,
                  width = 1500,
                  rowGetter = p.rowGetter _,
                  headerClassName = "headerColumn",
                  headerHeight = 30),
                columns: _*)
            )
          )
        )
      )
    }
    .build

  def apply(p: ModelProxy[GlobalLog]): Unmounted[Props, Unit, Unit] = component(Props(p))
}
