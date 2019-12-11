// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.common

import cats._
import cats.implicits._
import cats.data.NonEmptyList
import monocle.macros.Lenses
import monocle.{ Prism, Traversal }

/**
  * Minimal zipper based on scalaz's implementation
  * This is only meant for small collections. performance has not been optimized
  */
@Lenses
final case class Zipper[A](lefts: List[A], focus: A, rights: List[A]) {

  /**
    * Modify the focus
    */
  def modify(f: A => A): Zipper[A] = copy(lefts, f(focus), rights)

  /**
    * Modify the focus
    */
  def modifyP(p: Prism[A, A]): Zipper[A] =
    p.getOption(focus).map(f => copy(lefts, f, rights)).getOrElse(this)

  /**
    * Find and element and focus if successful
    */
  def findFocusP(p: PartialFunction[A, Boolean]): Option[Zipper[A]] =
    findFocus(p.lift.andThen(_.getOrElse(false)))

  /**
    * How many items are in the zipper
    */
  def length: Int = lefts.length + 1 + rights.length

  /**
    * Find and element and focus if successful
    */
  def findFocus(p: A => Boolean): Option[Zipper[A]] =
    if (p(focus)) this.some
    else {
      val indexLeft  = lefts.indexWhere(p)
      val indexRight = rights.indexWhere(p)
      if (indexLeft === -1 && indexRight === -1) {
        none
      } else if (indexLeft >= 0) {
        (lefts.splitAt(indexLeft) match {
          case (Nil, h :: t)      =>
            Zipper(Nil, h, t ::: focus :: rights)
          case (x :: Nil, i :: l) =>
            Zipper(List(x), i, l ::: focus :: rights)
          case (x, i :: l)        =>
            Zipper(x, i, (focus :: l.reverse).reverse ::: rights)
          case _                  =>
            this
        }).some
      } else {
        (rights.splitAt(indexRight) match {
          case (Nil, List(y)) =>
            Zipper((focus :: lefts.reverse).reverse, y, Nil)
          case (x, h :: t)    =>
            Zipper((focus :: lefts.reverse).reverse ::: x, h, t)
          case _              =>
            this
        }).some
      }
    }

  def exists(p: A => Boolean): Boolean =
    if (p(focus)) {
      true
    } else {
      lefts.exists(p) || rights.exists(p)
    }

  def find(p: A => Boolean): Option[A] =
    if (p(focus)) {
      focus.some
    } else {
      lefts.find(p).orElse(rights.find(p))
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

  /**
    * @typeclass Traverse
    * Based on traverse implementation for List
    */
  implicit val traverse: Traverse[Zipper] = new Traverse[Zipper] {
    override def traverse[G[_], A, B](fa: Zipper[A])(f: A => G[B])
      (implicit G: Applicative[G]): G[Zipper[B]] =
      (fa.lefts.traverse(f), f(fa.focus), fa.rights.traverse(f)).mapN {
        case (l, f, r) => Zipper(l, f, r)
      }

    override def foldLeft[A, B](fa: Zipper[A], b: B)(f: (B, A) => B): B =
      fa.toNel.foldLeft(b)(f)

    override def foldRight[A, B](fa: Zipper[A], lb: Eval[B])
      (f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      def loop(as: Vector[A]): Eval[B] =
        as match {
          case h +: t => f(h, Eval.defer(loop(t)))
          case _      => lb
        }

      Eval.defer(loop(fa.toList.toVector))
    }
  }

  def zipperT[A]: Traversal[Zipper[A], A] =
    Traversal.fromTraverse

  /**
    * Traversal zipper, Note this is unsafe as the predicate breaks some laws
    */
  def unsafeFilterZ[A](predicate: A => Boolean): Traversal[Zipper[A], A] =
    new Traversal[Zipper[A], A] {
      override def modifyF[F[_]: Applicative](f: A => F[A])
        (s: Zipper[A]): F[Zipper[A]] = {
        val lefts: F[List[A]] = s.lefts.collect {
          case x if predicate(x) => f(x)
          case x                 => x.pure[F]
        }.sequence
        val rights: F[List[A]] = s.rights.collect {
          case x if predicate(x) => f(x)
          case x                 => x.pure[F]
        }.sequence
        val focus: F[A] =
          if (predicate(s.focus)) f(s.focus) else s.focus.pure[F]
        (lefts, focus, rights).mapN { (l, f, r) =>
          Zipper(l, f, r)
        }
      }
    }

}
