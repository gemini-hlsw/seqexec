// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.services

import scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/**
  * JS Facade for HTML5 audio
  */
@js.native
@JSGlobal
class Audio(val src: String) extends js.Object {
  def play(): js.Any = js.native
  def pause(): js.Any = js.native
}
