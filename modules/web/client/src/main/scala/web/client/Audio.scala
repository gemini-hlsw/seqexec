// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/**
 * JS Facade for HTML5 audio
 */
@js.native
@JSGlobal
class Audio(val src: String) extends js.Object {
  def play(): js.Any                  = js.native
  def pause(): js.Any                 = js.native
  def canPlayType(tp: String): String = js.native
}

object Audio {
  implicit class AudioOps(val a: Audio) extends AnyVal {
    def canPlayMP3: Boolean  = a.canPlayType("audio/mpeg").nonEmpty
    def canPlayWebM: Boolean = a.canPlayType("audio/webm").nonEmpty
  }

  def selectPlayable(mp3: Audio, webm: Audio): Audio =
    if (mp3.canPlayMP3) {
      mp3
    } else if (webm.canPlayWebM) {
      webm
    } else mp3
}
