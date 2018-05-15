// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.web.client

import japgolly.scalajs.react.vdom.html_<^._
import cats.{Eq, Monoid}
import cats.implicits._

package style {
  final case class GStyle(htmlClass: String)

  object GStyle {
    val Zero: GStyle = GStyle("")

    implicit val eq: Eq[GStyle] = Eq.by(_.htmlClass)

    implicit val monoid: Monoid[GStyle] = new Monoid[GStyle] {
      override def empty = Zero
      def combine(x: GStyle, y: GStyle): GStyle = (x, y) match {
        case (x, y) if x === Zero && y === Zero => Zero
        case (x, y) if x =!= Zero && y === Zero => x
        case (x, y) if x === Zero && y =!= Zero => y
        case (x, y)                             => GStyle(s"$x $y")
      }
    }
  }
}

package object style {
  implicit final def geminiStyleToTagMod(s: GStyle): TagMod =
    ^.className := s.htmlClass
}
