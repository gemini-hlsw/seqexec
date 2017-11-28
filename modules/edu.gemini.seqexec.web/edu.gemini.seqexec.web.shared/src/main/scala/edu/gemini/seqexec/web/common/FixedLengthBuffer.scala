// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.web.common

import scalaz.{Show, Equal, Foldable, IsEmpty, Functor, MonadPlus}
import scalaz.syntax.std.boolean._
import scalaz.syntax.equal._
import scalaz.std.AllInstances._

object FixedLengthBuffer {
  private final case class FixedLengthBufferImpl[A](maxLength: Int, data: Vector[A]) extends FixedLengthBuffer[A] {
    // Sanity check
    require(maxLength >= data.length)
    require(maxLength >= 0)

    def append(element: A): FixedLengthBuffer[A] = {
      if (data.length === maxLength && data.length >= 0) {
        data match {
          case _ +: tail => FixedLengthBufferImpl[A](maxLength, tail :+ element)
          case _         => FixedLengthBufferImpl[A](maxLength, Vector(element))
        }
      } else {
        FixedLengthBufferImpl[A](maxLength, data :+ element)
      }
    }

    def toVector: Vector[A] =
      data

    def size: Int = data.size

    def isEmpty: Boolean = data.isEmpty

    def reverse: FixedLengthBuffer[A] = FixedLengthBufferImpl(maxLength, data.reverse)
  }

  def apply[A](maxLength: Int, initial: A*): Option[FixedLengthBuffer[A]] = {
    (maxLength > 0 && maxLength >= initial.length) option
      FixedLengthBufferImpl[A](maxLength, Vector(initial: _*))
  }

  def fromInt[A](maxLength: Int, initial: A*): Option[FixedLengthBuffer[A]] =
    apply[A](maxLength, initial: _*)

  def unsafeFromInt[A](maxLength: Int, initial: A*): FixedLengthBuffer[A] =
    fromInt[A](maxLength, initial: _*).getOrElse(sys.error(s"Invalid max length $maxLength, data length ${initial.length}"))

  def Zero[A]: FixedLengthBuffer[A] = FixedLengthBufferImpl[A](0, Vector.empty[A])

  implicit def show[A]: Show[FixedLengthBuffer[A]] = Show.showFromToString

  implicit def equal[A]: Equal[FixedLengthBuffer[A]] = Equal.equalA

  /**
   * @typeclass Functor
   */
  implicit val functor: Functor[FixedLengthBuffer] = new Functor[FixedLengthBuffer] {
    def map[B, C](fa: FixedLengthBuffer[B])(f: B => C): FixedLengthBuffer[C] = fa match {
        case FixedLengthBufferImpl(max, data) => FixedLengthBufferImpl[C](max, data.map(f))
      }
  }

  /**
   * @typeclass Foldable
   * Based on foldable implementation for Set
   */
  implicit val foldable: Foldable[FixedLengthBuffer] with MonadPlus[FixedLengthBuffer] with IsEmpty[FixedLengthBuffer] = new Foldable[FixedLengthBuffer] with MonadPlus[FixedLengthBuffer] with IsEmpty[FixedLengthBuffer] with Foldable.FromFoldr[FixedLengthBuffer] {
    override def length[A](fa: FixedLengthBuffer[A]) = fa.size
    def empty[A] = Zero[A]
    def plus[A](a: FixedLengthBuffer[A], b: => FixedLengthBuffer[A]) = FixedLengthBufferImpl(a.maxLength + b.maxLength, a.toVector ++ b.toVector)
    def isEmpty[A](fa: FixedLengthBuffer[A]) = fa.isEmpty

    @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.While"))
    def foldRight[A, B](fa: FixedLengthBuffer[A], z: => B)(f: (A, => B) => B) = {
      import scala.collection.mutable.ArrayStack
      // Faster using a mutable collection
      val s = new ArrayStack[A]
      fa.toVector.foreach(a => s += a)
      var r = z
      while (!s.isEmpty) {
        // Fixes stack overflow issue
        val w = r
        r = f(s.pop, w)
      }
      r
    }
    override def all[A](fa: FixedLengthBuffer[A])(f: A => Boolean) =
      fa.toVector.forall(f)
    override def any[A](fa: FixedLengthBuffer[A])(f: A => Boolean) =
      fa.toVector.exists(f)
    def point[A](a: => A): FixedLengthBuffer[A] = new FixedLengthBufferImpl(1, Vector(a))
    def bind[A, B](fa: FixedLengthBuffer[A])(f: A => FixedLengthBuffer[B]) = fa.toVector match {
      case y +: ys => plus(f(y), bind(new FixedLengthBufferImpl(fa.maxLength, ys))(f))
      case _       => Zero[B]
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
  def append(element: A): FixedLengthBuffer[A]

  /**
   * Max length of the list
   */
  def maxLength: Int

  /**
   * Converts the buffer to a list
   */
  def toVector: Vector[A]

  /**
   * Size of the data
   */
  def size: Int

  /**
   * Indicates if the buffer is empty
   */
  def isEmpty: Boolean

  /**
   * Returns a buffer with the values reversed
   */
  def reverse: FixedLengthBuffer[A]
}
