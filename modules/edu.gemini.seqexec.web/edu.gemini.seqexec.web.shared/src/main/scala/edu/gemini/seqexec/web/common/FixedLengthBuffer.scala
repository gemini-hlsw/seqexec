// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.web.common

import scalaz.{Show, Equal, Functor}
import scalaz.syntax.std.boolean._

object FixedLengthBuffer {
  private final case class FixedLengthBufferImpl[A](maxLength: Int, data: Vector[A]) extends FixedLengthBuffer[A] {
    // Sanity check
    require(maxLength >= data.length)
    require(maxLength > 0)

    def append(element: A): FixedLengthBuffer[A] = {
      if (data.length == maxLength && data.length >= 0) {
        FixedLengthBufferImpl[A](maxLength, data.tail :+ element)
      } else {
        FixedLengthBufferImpl[A](maxLength, data :+ element)
      }
    }

    def toVector: Vector[A] =
      data
  }

  def apply[A](maxLength: Int, initial: A*): Option[FixedLengthBuffer[A]] = {
    (maxLength > 0 && maxLength >= initial.length) option
      FixedLengthBufferImpl[A](maxLength, Vector(initial: _*))
  }

  def fromInt[A](maxLength: Int): Option[FixedLengthBuffer[A]] =
    apply[A](maxLength)

  def unsafeFromInt[A](maxLength: Int): FixedLengthBuffer[A] =
    fromInt[A](maxLength).getOrElse(sys.error(s"Invalid max length $maxLength"))

  implicit def show[A]: Show[FixedLengthBuffer[A]] = Show.showFromToString

  implicit def equal[A]: Equal[FixedLengthBuffer[A]] = Equal.equalA

  implicit def functor[A]: Functor[FixedLengthBuffer] = new Functor[FixedLengthBuffer] {
    def map[B, C](fa: FixedLengthBuffer[B])(f: B => C): FixedLengthBuffer[C] = fa match {
        case FixedLengthBufferImpl(max, data) => FixedLengthBufferImpl[C](max, data.map(f))
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
}
