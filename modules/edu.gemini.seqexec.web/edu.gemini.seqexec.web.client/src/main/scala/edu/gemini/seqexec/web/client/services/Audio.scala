package edu.gemini.seqexec.web.client.services

import scalajs.js

@js.native
class Audio(val src: String) extends js.Object {
  def play(): js.Any = js.native
  def pause(): js.Any = js.native
}
