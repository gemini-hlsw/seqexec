// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.collections.menu.MenuIcon
import react.semanticui.collections.menu._
import react.semanticui.sizes._
import react.semanticui.views.item._
import seqexec.web.client.actions.UpdateSessionFilter
import seqexec.web.client.circuit._
import seqexec.web.client.icons._
import seqexec.web.client.model.ObsClass
import seqexec.web.client.model.SessionQueueFilter

/**
  * Container for the queue table
  */
object SessionQueueTableFilter {
  private val filterConnect =
    SeqexecCircuit.connect(SeqexecCircuit.sessionQueueFilterReader)

  def onlyDayTime: Callback =
    SeqexecCircuit.dispatchCB(UpdateSessionFilter(SessionQueueFilter.obsClass.modify {
      case ObsClass.Daytime => ObsClass.All
      case _                => ObsClass.Daytime
    }))

  def onlyNightTime: Callback =
    SeqexecCircuit.dispatchCB(UpdateSessionFilter(SessionQueueFilter.obsClass.modify {
      case ObsClass.Nighttime => ObsClass.All
      case _                  => ObsClass.Nighttime
    }))

  private val component = ScalaComponent
    .builder[Unit]("SessionQueueTableFilter")
    .stateless
    .render_P(_ =>
      React.Fragment(
        filterConnect { f =>
          val filter = f()
          <.div(
            Menu(icon     = MenuIcon.Icon,
                 attached = MenuAttached.Attached,
                 compact  = true,
                 size     = Tiny,
                 clazz    = SeqexecStyles.filterPane)(
              Item(as    = "a",
                   clazz = SeqexecStyles.filterActiveButton.when_(filter.dayTimeSelected))(
                IconSun,
                ^.onClick --> onlyDayTime,
                " Daytime"
              ),
              Item(as    = "a",
                   clazz = SeqexecStyles.filterActiveButton.when_(filter.nightTimeSelected))(
                IconMoon,
                ^.onClick --> onlyNightTime,
                " Nighttime"
              )
            )
          )
        }
      )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(): Unmounted[Unit, Unit, Unit] =
    component()

}
