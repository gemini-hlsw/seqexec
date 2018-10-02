// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.queue

// import diode.react.ReactConnectProxy
// import cats.implicits._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Callback
// import japgolly.scalajs.react.CallbackTo
import japgolly.scalajs.react.ScalaComponent
// import japgolly.scalajs.react.CatsReact
// import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.component.Scala.Unmounted
// import japgolly.scalajs.react.extra.Reusability
// import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
// import gem.Observation
// import mouse.all._
// import seqexec.model.SequenceState
// import seqexec.web.client.circuit._
// import seqexec.web.client.actions.RequestCancelPause
// import seqexec.web.client.actions.RequestPause
// import seqexec.web.client.actions.RequestSync
// import seqexec.web.client.actions.RequestRun
// import seqexec.web.client.model.RunOperation
// import seqexec.web.client.model.PauseOperation
// import seqexec.web.client.model.SyncOperation
import seqexec.web.client.components.SeqexecStyles
// import seqexec.web.client.semanticui.elements.icon.Icon
// import seqexec.web.client.semanticui.elements.icon.Icon.IconRefresh
import seqexec.web.client.semanticui.elements.icon.Icon.IconCloneOutline
import seqexec.web.client.semanticui.controlButton
// import seqexec.web.client.semanticui.elements.icon.Icon.IconPause
// import seqexec.web.client.semanticui.elements.icon.Icon.IconBan
// import seqexec.web.client.reusability._
import web.client.style._

/**
  * Toolbar for logged in users
  */
object QueueToolbar {
  final case class Props() {
    def cmp: Unmounted[Props, Unit, Unit] = component(this)
  }

  private val component = ScalaComponent
    .builder[Props]("QueueToolbar")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui grid",
        <.div(
          ^.cls := "row",
          SeqexecStyles.shorterRow,
          <.div(
            ^.cls := "ui left floated column",
            controlButton(
              icon     = IconCloneOutline,
              color    = "blue",
              onClick  = Callback.empty,
              disabled = false,
              tooltip  = "Add all daytime calibrations on the session queue",
              text     = "Add all day cal"
            )
          )
        )
    ))
    .build

}
