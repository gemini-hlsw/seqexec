// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import japgolly.scalajs.react.extra.TimerSupport
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.CtorType
import japgolly.scalajs.react.{ BackendScope, Callback, Reusability }
import japgolly.scalajs.react.component.Scala
import monocle.macros.Lenses
import react.common._
import scala.concurrent.duration._
import scala.math.max
import scala.math.min

abstract class SmoothProgressBarProps[A](override val component: Scala.Component[A, _, _, CtorType.Props])
    extends ReactProps[A](component) {
  val value: Int
  val maxValue: Int
  val stopping: Boolean
  val paused: Boolean
}

trait SmoothProgressBar[P <: SmoothProgressBarProps[P]] {
  @Lenses
  protected case class State(value: Int, prevStopping: Boolean, prevPaused: Boolean)

  protected object State {
    def fromProps(p: P): State = State(p.value, p.stopping, p.paused)
  }

  implicit protected val stateReuse: Reusability[State] = Reusability.derive[State]

  protected def deriveNewState(props: P, state: State): State = 
    if (props.paused =!= state.prevPaused || props.stopping =!= state.prevStopping) {
      state
    } else if (props.stopping) {
      state
    } else {
      State(max(props.value, state.value), state.prevStopping, state.prevPaused)
    }

  protected class Backend(b: BackendScope[P, State]) extends TimerSupport {
    private val periodUpdate: Int = 50
    // This depends on the server side frequency of updates
    private val remoteUpdatePeriod: Int = 1000

    def setupTimer: Callback =
      setInterval(tickTotal, periodUpdate.millisecond)

    def tickTotal: Callback = 
      b.props.zip(b.state) >>= {
        case (p, s) =>
          val next = min(s.value + periodUpdate, p.value + remoteUpdatePeriod)
          (b.setStateL(State.value)(min(p.maxValue, next)))
            .when(!p.paused && !p.stopping).void
      }
  }

}
