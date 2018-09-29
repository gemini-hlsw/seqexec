// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import seqexec.web.client.circuit._
// import cats.implicits._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.Reusability
import seqexec.web.client.model.SessionQueueFilter
import seqexec.web.client.model.ObsClass
import seqexec.web.client.actions.UpdateSessionFilter
import web.client.style._

/**
  * Container for the queue table
  */
object SessionQueueTableFilter {
  private val filterConnect =
    SeqexecCircuit.connect(SeqexecCircuit.sessionQueueFilterReader)

  def onlyDayTime: Callback =
    SeqexecCircuit.dispatchCB(
      UpdateSessionFilter(SessionQueueFilter.obsClass.modify {
        case ObsClass.Daytime => ObsClass.All
        case _                => ObsClass.Daytime
      }))

  def onlyNightTime: Callback =
    SeqexecCircuit.dispatchCB(
      UpdateSessionFilter(SessionQueueFilter.obsClass.modify {
        case ObsClass.Nighttime => ObsClass.All
        case _                  => ObsClass.Nighttime
      }))

  private val component = ScalaComponent
    .builder[Unit]("SessionQueueTableFilter")
    .stateless
    .render_P(
      _ =>
        filterConnect { f =>
          val filter = f()
          <.div(
            ^.cls := "ui icon bottom attached compact tiny menu",
            SeqexecStyles.filterPane,
            <.a(
              ^.cls := "item",
              SeqexecStyles.filterActiveButton.when(filter.dayTimeSelected),
              <.i(
                ^.cls := "sun icon"
              ),
              ^.onClick --> onlyDayTime,
              " Daytime"
            ),
            <.a(
              ^.cls := "item",
              SeqexecStyles.filterActiveButton.when(filter.nightTimeSelected),
              <.i(
                ^.cls := "moon icon"
              ),
              ^.onClick --> onlyNightTime,
              " Nighttime"
            )
          )
      }
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(): Unmounted[Unit, Unit, Unit] =
    component()

}
