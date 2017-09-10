// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.services

import scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import org.scalajs.dom.raw.AudioContext
import scala.scalajs.js.timers._

/**
  * JS Facade for HTML5 audio
  */
@js.native
@JSGlobal
class Audio(val src: String) extends js.Object {
  def play(): js.Any = js.native
  def pause(): js.Any = js.native
}

object Beep {
  private val ctx = new AudioContext
    private val oscillator = ctx.createOscillator
    private val gain = ctx.createGain

    oscillator.connect(gain)
    gain.connect(ctx.destination)

    // Volume 0 at start
    gain.gain.value = 0
    // frequency
    oscillator.frequency.value = 3020
    // type
    oscillator.`type` = "sine"
    // start
    oscillator.start()

  /** Emits a beep with the Web Audio API */
  def beep(): SetTimeoutHandle = {
    gain.gain.value = 1
    setTimeout(0.05) { gain.gain.value = 0 }
  }
}
