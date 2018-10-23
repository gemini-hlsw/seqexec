// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client

import cats.Eq
import cats.Monoid
import cats.implicits._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.Reusability

package style {
  final case class GStyle(htmlClasses: List[String]) {
    val htmlClass: String = htmlClasses.mkString(" ")
  }

  object GStyle {
    def fromString(htmlClass: String): GStyle = GStyle(List(htmlClass))

    val Zero: GStyle = GStyle(Nil)

    implicit val eq: Eq[GStyle] = Eq.by(_.htmlClass)

    implicit val reuse: Reusability[GStyle] = Reusability.by(_.htmlClass)

    implicit val monoid: Monoid[GStyle] =
      Monoid[List[String]].imap(GStyle.apply)(_.htmlClasses)
  }
}

package object style {
  implicit final def geminiStyleToTagMod(s: GStyle): TagMod =
    ^.className := s.htmlClass
}
