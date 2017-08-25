// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.web.common

import scalaz.{Show, Equal, Functor}

object FixedLengthBuffer {
  private final case class FixedLengthBufferImpl[A](maxLength: Int, data: List[A]) extends FixedLengthBuffer[A] {
    def append(element: A): FixedLengthBuffer[A] = {
      if (data.length == maxLength) {
        FixedLengthBufferImpl[A](maxLength, element :: data.reverse.tail.reverse)
      } else {
        FixedLengthBufferImpl[A](maxLength, element :: data)
      }
    }

    def toList: List[A] =
      data.reverse
  }

  private object FixedLengthBufferImpl {
    def apply[A](maxLength: Int, initial: A*): FixedLengthBuffer[A] = {
      // Sanity check
      require(maxLength >= initial.length)
      new FixedLengthBufferImpl[A](maxLength, List(initial: _*))
    }

  }

  def apply[A](maxLength: Int): FixedLengthBuffer[A] = FixedLengthBufferImpl[A](maxLength)

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
  def toList: List[A]
}
