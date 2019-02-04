// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.util

import cats.data.NonEmptyList

/**
 * A placeholder zipper for now. Stolen from https://github.com/tpolecat/tuco/blob/master/modules/core/src/main/scala/tuco/util/zipper.scala
 */
final case class Zipper[A](lefts: List[A], focus: A, rights: List[A]) {

  def previous: Option[Zipper[A]] =
    lefts match {
      case Nil     => None
      case a :: as => Some(Zipper(as, a, focus :: rights))
    }

  def next: Option[Zipper[A]] =
    rights match {
      case Nil     => None
      case a :: as => Some(Zipper(focus :: rights, a, as))
    }

}

object Zipper {
  def single[A](a: A): Zipper[A] = Zipper(Nil, a, Nil)
  def fromNonEmptyList[A](nel: NonEmptyList[A]): Zipper[A] = Zipper(Nil, nel.head, nel.tail)
}
