// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client

import japgolly.scalajs.react.vdom.html_<^.VdomElement

trait ReactProps {
  @inline def render: VdomElement
}

object ReactProps {
  implicit def props2component(p: ReactProps): VdomElement = p.render
}
