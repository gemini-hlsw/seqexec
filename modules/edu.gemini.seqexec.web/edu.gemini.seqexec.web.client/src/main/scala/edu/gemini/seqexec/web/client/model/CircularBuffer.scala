// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package CircularBuffer

import scala.reflect.ClassTag
import scalaz.{Show, Equal}

object CircularBuffer {
  private final case class CircularBufferImpl[A: ClassTag](maxLength: Int, length: Int, position: Int, private val data: Array[A]) extends CircularBuffer[A] {

    def append(element: A): CircularBuffer[A] = {
      if (data.length == maxLength) {
        data(0) = element
        CircularBufferImpl[A](maxLength, length, 0, data)
      } else {
        data(position + 1) = element
        CircularBufferImpl[A](maxLength, length + 1, position + 1, data)
      }
    }
  }

  private object CircularBufferImpl {
    def apply[A: ClassTag](maxLength: Int, initial: A*): CircularBuffer[A] = {
      val data: Array[A] = Array.ofDim(maxLength)
      // Sanity check
      require(maxLength >= initial.length)
      initial.zipWithIndex.map {
        case (a, i) =>
          data(i) = a
      }
      val position = initial.length
      CircularBufferImpl(maxLength, position, position, data)
    }

  }

  def apply[A: ClassTag](maxLength: Int): CircularBuffer[A] = CircularBufferImpl[A](maxLength)

  implicit def show[A]: Show[CircularBuffer[A]] = Show.showFromToString

  implicit def equal[A]: Equal[CircularBuffer[A]] = Equal.equalA

  // implicit def foldable[A]: Foldable[CircularBuffer] = new Foldable[CircularBuffer] with Foldable.FromFoldMap[CircularBuffer] {
  //   override def foldMap[B, C: ClassTag](fa: CircularBuffer[B])(f: B => C)(implicit F: Monoid[C]) = fa match {
  //     case CircularBufferImpl(ml, l, p, data) => CircularBufferImpl[C](ml, l, p, data.foldMap(f).toArray)
  //   }
  // }
}

/**
 * Immutable Circular Buffer
 */
trait CircularBuffer[A] {
  /**
   * Append elements to the buffer
   */
  def append(element: A): CircularBuffer[A]
}
