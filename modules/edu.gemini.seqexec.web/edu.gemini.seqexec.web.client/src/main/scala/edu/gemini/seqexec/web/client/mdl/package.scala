package edu.gemini.seqexec.web.client

import japgolly.scalajs.react.{TopNode, ReactComponentU}
import japgolly.scalajs.react.vdom.all._

package object mdl {
  implicit class MaterialAble(val elem: ReactTag) extends AnyVal {
    def mdl: ReactComponentU[(ReactTag, Boolean), Unit, Unit, TopNode] = {
      MaterialComponent((elem, false))
    }

    def mdl(children: Boolean): ReactComponentU[(ReactTag, Boolean), Unit, Unit, TopNode] = {
      MaterialComponent((elem, children))
    }
  }
}