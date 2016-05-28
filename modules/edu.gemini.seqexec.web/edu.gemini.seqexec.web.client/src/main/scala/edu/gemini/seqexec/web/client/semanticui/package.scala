package edu.gemini.seqexec.web.client

import japgolly.scalajs.react.vdom.prefix_<^._

package object semanticui {
  // Custom attributes used by SemanticUI
  val dataTab = "data-tab".reactAttr

  sealed trait Size

  object Size {
    case object NotSized extends Size
    case object Tiny extends Size
    case object Mini extends Size
    case object Medium extends Size
    case object Small extends Size
    case object Large extends Size
    case object Big extends Size
    case object Huge extends Size
    case object Massive extends Size
  }
}
