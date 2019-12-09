// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import japgolly.scalajs.react.extra.TimerSupport
import japgolly.scalajs.react.{BackendScope, Callback, Reusability}
import monocle.macros.Lenses
import react.common._
import scala.concurrent.duration._
import scala.math.max
import scala.math.min

trait SmoothProgressBarProps extends ReactProps {
  val value: Int
  val maxValue: Int
  val stopping: Boolean
  val paused: Boolean
}

trait SmoothProgressBar[P <: SmoothProgressBarProps] {
  @Lenses
  protected case class State(maxValue: Int, value: Int, skipStep: Boolean)

  protected object State {
    def fromProps(p: P): State = State(p.maxValue, p.value, skipStep = false)
  }

  implicit protected val stateReuse: Reusability[State] = Reusability.derive[State]

  protected class Backend(b: BackendScope[P, State]) extends TimerSupport {
    private val periodUpdate: Int = 50
    // This depends on the server side frequency of updates
    private val remoteUpdatePeriod: Int = 1000

    def setupTimer: Callback =
      setInterval(tickTotal, periodUpdate.millisecond)

    def newStateFromProps(prev: P, next: P): Callback =
      b.modState { s =>
        if (prev.paused =!= next.paused || prev.stopping =!= next.stopping) {
          s
        } else if (next.stopping) {
          s
        } else {
          State(next.maxValue, max(next.value, s.value), s.value > next.value)
        }
      }

    def tickTotal: Callback = b.props.zip(b.state) >>= {
      case (p, s) =>
        val next = min(s.value + periodUpdate, p.value + remoteUpdatePeriod)
        b.modState(State.value.set(min(p.maxValue, next)))
         .when(!s.skipStep && !p.paused && !p.stopping) *>
          b.modState(State.skipStep.set(false))
    }
  }

}
