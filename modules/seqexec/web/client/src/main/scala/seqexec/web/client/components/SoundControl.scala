// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.Reusability
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.actions.FlipSoundOnOff
import seqexec.web.client.model.SoundSelection
import seqexec.web.client.semanticui.Size
import seqexec.web.client.semanticui.elements.button.Button
import seqexec.web.client.semanticui.elements.icon.Icon._
import seqexec.web.client.services.WebpackResources._
import seqexec.web.client.reusability._
import web.client.Audio

/**
  * Button to toggle sound on/off
  */
object SoundControl {
  private val SoundOn = Audio.selectPlayable(new Audio(SoundOnMP3.resource),
                                             new Audio(SoundOnWebM.resource))

  final case class Props(sound: SoundSelection)

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private def flipSound: Callback =
    SeqexecCircuit.dispatchCB(FlipSoundOnOff)

  private val component = ScalaComponent
    .builder[Props]("SoundControl")
    .stateless
    .render_P { p =>
      val icon = p.sound match {
        case SoundSelection.SoundOn  => IconVolumeUp
        case SoundSelection.SoundOff => IconVolumeOff
      }
      val soundClick = p.sound match {
        case SoundSelection.SoundOn  => Callback.empty
        case SoundSelection.SoundOff => Callback(SoundOn.play())
      }
      Button(
        icon     = icon.some,
        inverted = true,
        size     = Size.Medium,
        onClick  = soundClick *> flipSound)
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
