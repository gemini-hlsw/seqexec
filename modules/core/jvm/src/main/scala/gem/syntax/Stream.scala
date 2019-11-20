// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.syntax

import fs2.{ Chunk, Pull, Stream }

final class StreamOps[F[_], O](val self: Stream[F, O]) {

  /** A filter/fold combination that uses the accumulated result of folding
    * over the elements since the last element was emitted to determine whether
    * the current element under examination should be emitted.  The filter
    * begins with a provided value `z` for the accumulator.  Each successive
    * value is combined with the accumulated value using a function `f`.  If the
    * result of `f` matches the predicate `p` then the element is emitted and
    * the accumulated value is reset to `z`.
    *
    * @example {{{
    * scala> val s = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    * s: fs2.Stream[fs2.Pure,Int] = Stream(..)
    *
    * scala> s.filterOnFold(0)(_ + _, _ % 2 == 0).toList
    * res0: List[Int] = List(3, 4, 7, 8)
    * }}}
    *
    *
    * @param z initial value for the accumulator
    * @param f function that combines the current value with the accumulated
    *          value
    * @param p predicate that determines whether the current element should
    *          be emitted (after being combined with the accumulated value using
    *          `f`)
    *
    * @tparam A type of the accumulated value
    *
    * @return a Stream filtered by the accumulator and predicate
    */
  def filterOnFold[A](z: A)(f: (A, O) => A, p: A => Boolean): Stream[F, O] = {

    def go(a: A, s: Stream[F,O]): Pull[F,O,Unit] =

      s.pull.uncons.flatMap {
        case None           =>
          Pull.pure(())

        case Some((hd: Chunk[O], tl: Stream[F, O])) =>
          hd.foldLeft((Vector.empty[O], a)) { case ((matching, a), o) =>
            val aʹ = f(a, o)
            if (p(aʹ)) (matching :+ o, z) else (matching, aʹ)
          } match { case (matching, aʹ) =>
            Pull.output(Chunk.vector(matching)) >> go(aʹ, tl)
          }
      }

    go(z, self).stream

  }
}

trait ToStreamOps {
  implicit def ToStreamOps[F[_], O](s: Stream[F, O]): StreamOps[F, O] =
    new StreamOps[F,O](s)
}

object stream extends ToStreamOps
