// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.util

import cats._
import cats.implicits._
import cats.data.NonEmptyList

/**
 * A placeholder zipper for now. Stolen and modified from
 * https://github.com/tpolecat/tuco/blob/master/modules/core/src/main/scala/tuco/util/zipper.scala
 */
final case class Zipper[A](lefts: List[A], focus: A, rights: List[A]) {

  def toList: List[A] =
    lefts.reverse ++ (focus :: rights)

  def toNel: NonEmptyList[A] =
    NonEmptyList.fromListUnsafe(toList) // we know the list has at least one element, the focus

  def previous: Option[Zipper[A]] =
    lefts match {
      case Nil     => None
      case a :: as => Some(Zipper(as, a, focus :: rights))
    }

  def next: Option[Zipper[A]] =
    rights match {
      case Nil     => None
      case a :: as => Some(Zipper(focus :: lefts, a, as))
    }

}

object Zipper {

  /**
   * @group Constructors
   */
  def single[A](a: A): Zipper[A] =
    Zipper(Nil, a, Nil)

  /**
   * @group Constructors
   */
  def fromNonEmptyList[A](nel: NonEmptyList[A]): Zipper[A] =
    Zipper(Nil, nel.head, nel.tail)

  /**
   * @group Instances
   */
  implicit def EqZipper[A: Eq]: Eq[Zipper[A]] =
    Eq.instance { (za, zb) =>
      (za.focus === zb.focus)   &&
        (za.lefts === zb.lefts) &&
        (za.rights === zb.rights)
    }

  /**
   * Applicative defined as Applicative[List] where the focus is the element
   * produced by applying the focus of Zipper[A => B] to the focus of Zipper[A].
   *
   * @group Instances
   */
  implicit val ApplicativeZipper: Applicative[Zipper] =
    new Applicative[Zipper] {

      def pure[A](a: A): Zipper[A] =
        Zipper(Nil, a, Nil)

      def ap[A, B](zf: Zipper[A => B])(za: Zipper[A]): Zipper[B] = {
        val as = za.toList

        Zipper(
          za.lefts.map(zf.focus)  ++ zf.lefts.reverse.flatMap(f => as.map(f)).reverse,
          zf.focus(za.focus),
          za.rights.map(zf.focus) ++ zf.rights.flatMap(f => as.map(f))
        )
      }
    }

  // NonEmptyTraverse[Zipper] should be do-able as well.

}
