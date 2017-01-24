package edu.gemini.seqexec.web.client.semanticui.elements.table

import japgolly.scalajs.react.{ReactComponentB, ReactNode}
import japgolly.scalajs.react.vdom.prefix_<^._
import edu.gemini.seqexec.web.client.semanticui._

import scalaz.syntax.equal._

object TableHeader {
  case class Props(collapsing: Boolean = false,
    colSpan: Option[Int] = None,
    aligned: Aligned = Aligned.None,
    width: Width = Width.None)

  val component = ReactComponentB[Props]("th")
    .stateless
    .renderPC((_, p, c) =>
      <.th(
        ^.classSet(
          "collapsing"     -> p.collapsing,
          "one wide"       -> (p.width === Width.One),
          "two wide"       -> (p.width === Width.Two),
          "three wide"     -> (p.width === Width.Three),
          "four wide"      -> (p.width === Width.Four),
          "five wide"      -> (p.width === Width.Five),
          "six wide"       -> (p.width === Width.Six),
          "seven wide"     -> (p.width === Width.Seven),
          "eight wide"     -> (p.width === Width.Eight),
          "nine wide"      -> (p.width === Width.Nine),
          "ten wide"       -> (p.width === Width.Ten),
          "eleven wide"    -> (p.width === Width.Eleven),
          "twelve wide"    -> (p.width === Width.Twelve),
          "thirteen wide"  -> (p.width === Width.Thirteen),
          "fourteen wide"  -> (p.width === Width.Fourteen),
          "fifteen wide"   -> (p.width === Width.Fifteen),
          "sixteen wide"   -> (p.width === Width.Sixteen),
          "left aligned"   -> (p.aligned === Aligned.Left),
          "center aligned" -> (p.aligned === Aligned.Center),
          "right aligned"  -> (p.aligned === Aligned.Right)
        ),
        p.colSpan.map(^.colSpan := _),
        c
      )
    ).build

  def apply(p: Props, children: ReactNode*) = component(p, children)
}
