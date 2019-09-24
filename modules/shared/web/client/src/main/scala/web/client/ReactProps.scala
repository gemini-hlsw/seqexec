package web.client

import japgolly.scalajs.react.vdom.html_<^.VdomElement

trait ReactProps {
  @inline def render: VdomElement
}

object ReactProps {
  implicit def props2component(p: ReactProps): VdomElement = p.render
}
