// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.common

import cats.implicits._
import cats.data.NonEmptyList
import cats._

/**
 * Minimal zipper based on scalaz's implementation
 * This is only meant for small collections. performance has not been optimized
 */
final case class Zipper[A](lefts: List[A], focus: A, rights: List[A]) {
  /**
    * Modify the focus
    */
  def modify(f: A => A): Zipper[A] = copy(lefts, f(focus), rights)

  /**
    * Find and element and focus if successful
    */
  def findFocus(p: A => Boolean): Option[Zipper[A]] =
    if (p(focus)) this.some
    else {
      val indexLeft = lefts.indexWhere(p)
      val indexRight = rights.indexWhere(p)
      if (indexLeft === -1 && indexRight === -1) {
        none
      } else if (indexLeft >= 0) {
        (lefts.splitAt(indexLeft) match {
          case (Nil, h :: t)      => Zipper(Nil, h, t ::: focus :: rights)
          case (x :: Nil, i :: l) => Zipper(List(x), i, l ::: focus :: rights)
          case (x, i :: l)        => Zipper(x, i, (focus :: l.reverse).reverse ::: rights)
          case _                  => this
        }).some
      } else {
        (rights.splitAt(indexRight) match {
          case (Nil, List(y)) => Zipper((focus :: lefts.reverse).reverse, y, Nil)
          case (x, h :: t)    => Zipper((focus :: lefts.reverse).reverse ::: x, h, t)
          case _              => this
        }).some
      }
    }

  def exists(p: A => Boolean): Boolean = {
    if (p(focus)) true else {
      lefts.exists(p) || rights.exists(p)
    }
  }

  def find(p: A => Boolean): Option[A] = {
    if (p(focus)) Some(focus) else {
      lefts.find(p).orElse(rights.find(p))
    }
  }

  def withFocus: Zipper[(A, Boolean)] =
    Zipper(lefts.map((_, false)), (focus, true), rights.map((_, false)))

  def toList: List[A] = (focus :: lefts.reverse).reverse ::: rights

  def toNel: NonEmptyList[A] = NonEmptyList.fromListUnsafe(toList)
}

object Zipper {
  def fromNel[A](ne: NonEmptyList[A]): Zipper[A] =
    Zipper(Nil, ne.head, ne.tail)

  implicit def equal[A: Eq]: Eq[Zipper[A]] = Eq.instance { (a, b) =>
    a.focus === b.focus && a.lefts === b.lefts && a.rights === b.rights
  }

  implicit val functor: Functor[Zipper] = new Functor[Zipper] {
    def map[A, B](fa: Zipper[A])(f: A => B): Zipper[B] =
      Zipper(fa.lefts.map(f), f(fa.focus), fa.rights.map(f))
  }
}
