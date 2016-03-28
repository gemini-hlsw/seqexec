package edu.gemini.seqexec.web.client.mdl

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react.CompScope

import scala.scalajs.js

object MaterialComponent {
  type ScopeType = CompScope.DuringCallbackM[(ReactTag, Boolean), Unit, Unit, TopNode]

  val rc = ReactComponentB[(ReactTag, Boolean)]("MaterialComponent")
    .render_P(props => props._1)
    .componentDidMount(afterMount)
    //.componentDidUpdate(afterUpdate)
    .build

  def apply(props: (ReactTag, Boolean)): ReactComponentU[(ReactTag, Boolean), Unit, Unit, TopNode] = {
    rc(props)
  }

  private def upgrade(scope: ScopeType): Callback = Callback {
    js.Dynamic.global.componentHandler.upgradeElement(scope.getDOMNode())
    if (scope.props._2) {
      val children = scope.getDOMNode().children
      (0 until children.length).foreach(i =>
        js.Dynamic.global.componentHandler.upgradeElement(children(i))
      )
    }
  }

  def afterMount(scope: ScopeType) = {
    upgrade(scope)
  }

  def afterUpdate(scope: ScopeType, props: (ReactTag, Boolean), state: Unit) = {
    upgrade(scope)
  }
}
