// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.optics

import cats.arrow.Category
import monocle.Iso

/**
 * Composition of a `SplitMono` and a `SplitEpi`, yielding an even weaker structure where neither
 * `get andThen reverseGet` and `reverseGet andThen get` is an identity but both are idempotent.
 */
final case class Wedge[A, B](get: A => B, reverseGet: B => A) {

  /** Swapping `get` and `reverseGet` yields a `Wedge`. */
  def reverse: Wedge[B, A] =
    Wedge(reverseGet, get)

  /** Compose with another Wedge. */
  def composeWedge[C](f: Wedge[B, C]): Wedge[A, C] =
    Wedge(get andThen f.get, reverseGet compose f.reverseGet)

  /** Compose with an Iso. */
  def composeIso[C](f: Iso[B, C]): Wedge[A, C] =
    Wedge(get andThen f.get, reverseGet compose f.reverseGet)

  /** Alias to composeWedge. */
  def ^<-![C](f: Wedge[B, C]): Wedge[A, C] =
    composeWedge(f)

  /** Alias to composeIso. */
  def ^<->[C](f: Iso[B, C]): Wedge[A, C] =
    composeIso(f)

  /** Wedge is an invariant functor over A. */
  def imapA[C](f: A => C, g: C => A): Wedge[C, B] =
    Wedge(g andThen get, reverseGet andThen f)

  /** Wedge is an invariant functor over B. */
  def imapB[C](f: C => B, g: B => C): Wedge[A, C] =
    Wedge(get andThen g, f andThen reverseGet)

  /** Normalize A via a round-trip through B. */
  def normalizeA(a: A): A =
    (get andThen reverseGet)(a)

  /** Normalize B via a round-trip through A. */
  def normalizeB(b: B): B =
    (get compose reverseGet)(b)

}

object Wedge {

  /** An Iso is trivially a Wedge. */
  def fromIso[A, B](p: Iso[A, B]): Wedge[A, B] =
    Wedge(p.get, p.reverseGet)

  /** Wedge forms a category. */
  implicit def WedgeCategory: Category[Wedge] =
    new Category[Wedge] {
      def id[A] = Wedge(identity, identity)
      def compose[A, B, C](f: Wedge[B, C], g: Wedge[A, B]): Wedge[A, C] = g ^<-! f
    }

}
