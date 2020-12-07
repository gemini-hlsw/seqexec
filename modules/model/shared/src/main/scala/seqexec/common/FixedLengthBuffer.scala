// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.common

import cats.Applicative
import cats.Eq
import cats.Eval
import cats.Order
import cats.Traverse
import cats.data.Chain
import cats.data.Chain._
import cats.syntax.all._
import mouse.all._

object FixedLengthBuffer {
  private final case class FixedLengthBufferImpl[A](maxLength: Int, data: Chain[A]) extends FixedLengthBuffer[A] {
    // Sanity check
    require(maxLength >= data.length)
    require(maxLength >= 0)

    def append(element: A)(implicit ev: Order[A]): FixedLengthBuffer[A] = {
      if (data.length === maxLength.toLong && data.length >= 0) {
        data match {
          case _ ==: tail => FixedLengthBufferImpl[A](maxLength, tail :+ element)
          case _          => FixedLengthBufferImpl[A](maxLength, Chain(element))
        }
      } else {
        FixedLengthBufferImpl[A](maxLength, (data :+ element).distinct)
      }
    }

    def toChain: Chain[A] =
      data

    def isEmpty: Boolean = data.isEmpty

    def reverse: FixedLengthBuffer[A] = FixedLengthBufferImpl(maxLength, data.reverse)

    def flatMap[B](f: A => FixedLengthBuffer[B]): FixedLengthBuffer[B] =
      FixedLengthBufferImpl(maxLength, this.data.flatMap(f(_).toChain))

  }

  def apply[A](maxLength: Int, initial: A*): Option[FixedLengthBuffer[A]] = {
    (maxLength > 0 && maxLength >= initial.length) option
      FixedLengthBufferImpl[A](maxLength, Chain(initial: _*))
  }

  def fromInt[A](maxLength: Int, initial: A*): Option[FixedLengthBuffer[A]] =
    apply[A](maxLength, initial: _*)

  def unsafeFromInt[A](maxLength: Int, initial: A*): FixedLengthBuffer[A] =
    fromInt[A](maxLength, initial: _*).getOrElse(sys.error(s"Invalid max length $maxLength, data length ${initial.length}"))

  def Zero[A]: FixedLengthBuffer[A] = FixedLengthBufferImpl[A](0, Chain.empty[A])

  implicit def equal[A: Eq]: Eq[FixedLengthBuffer[A]] =
    Eq.by(x => (x.maxLength, x.toChain))

  /**
   * @typeclass Traverse
   * Based on traverse implementation for List
   */
  implicit val instance: Traverse[FixedLengthBuffer] = new Traverse[FixedLengthBuffer] {
    override def traverse[G[_], A, B](fa: FixedLengthBuffer[A])(f: A => G[B])(implicit G: Applicative[G]): G[FixedLengthBuffer[B]] =
      fa.toChain.traverse(f).map(FixedLengthBufferImpl(fa.maxLength, _))

    override def foldLeft[A, B](fa: FixedLengthBuffer[A], b: B)(f: (B, A) => B): B =
      fa.toChain.foldLeft(b)(f)

    override def foldRight[A, B](fa: FixedLengthBuffer[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      def loop(as: Chain[A]): Eval[B] =
        as match {
          case h ==: t => f(h, Eval.defer(loop(t)))
          case _       => lb
        }

      Eval.defer(loop(fa.toChain))
    }
  }

}

/**
 * Immutable Fixed Length Buffer. It will accumulate items in order
 * and discard old ones when full
 */
sealed trait FixedLengthBuffer[A] {
  /**
   * Append elements to the buffer
   */
  def append(element: A)(implicit ev: Order[A]): FixedLengthBuffer[A]

  /**
   * Max length of the list
   */
  def maxLength: Int

  /**
   * Converts the buffer to a list
   */
  def toChain: Chain[A]

  /**
   * Indicates if the buffer is empty
   */
  def isEmpty: Boolean

  /**
   * Returns a buffer with the values reversed
   */
  def reverse: FixedLengthBuffer[A]

  def flatMap[B](f: A => FixedLengthBuffer[B]): FixedLengthBuffer[B]
}
