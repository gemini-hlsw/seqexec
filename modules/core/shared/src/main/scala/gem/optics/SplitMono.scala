// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.optics

import cats.arrow.Category
import monocle.{ Fold, Getter, Iso }

/**
 * A split monomorphism, which we can think of as a weaker `Iso[A, B]` where `A` is a ''smaller''
 . type. So `get andThen reverseGet andThen` remains an identity but `reverseGet andThen get` is merely
 * idempotent (i.e., it normalizes values in `B`). The following statements hold:
 *
 *  - `reverseGet` is a ''retraction'' of `get`,
 *  - `get` is a ''section'' of `reverseGet`,
 *  - `A` is a ''retract'' of `B`,
 *  - the pair `(reverseGet, get)` is a ''splitting'' of the idempotent `reverseGet andThen get`.
 *
 * @param get  section of `reverseGet` such that `get andThen reverseGet` is an identity
 * @param reverseGet any function B => A
 * @see [[https://ncatlab.org/nlab/show/split+monomorphism Split Epimorphism]] at nLab
 */
final case class SplitMono[A, B](get: A => B, reverseGet: B => A) {

  /** Swapping `get` and `reverseGet` yields a `SplitEpi. */
  def reverse: SplitEpi[B, A] =
    SplitEpi(reverseGet, get)

  /** Compose with another SplitMono. */
  def composeSplitMono[C](f: SplitMono[B, C]): SplitMono[A, C] =
    SplitMono(get andThen f.get, reverseGet compose f.reverseGet)

  /** Compose with an Iso. */
  def composeIso[C](f: Iso[B, C]): SplitMono[A, C] =
    SplitMono(get andThen f.get, reverseGet compose f.reverseGet)

  /** View this SplitEpi as a Fold. */
  def asFold: Fold[A, B] =
    asGetter.asFold

  /** View this SplitMono as a Getter. */
  def asGetter: Getter[A, B] =
    Getter(get)

  /** View this SplitMono as a Wedge. */
  def asWedge: Wedge[A, B] =
    Wedge(get, reverseGet)

  /** Alias to composeSplitMono. */
  def ^<-![C](f: SplitMono[B, C]): SplitMono[A, C] =
    composeSplitMono(f)

  /** Alias to composeIso. */
  def ^<->[C](f: Iso[B, C]): SplitMono[A, C] =
    composeIso(f)

  /** SplitMono is an invariant functor over A. */
  def imapA[C](f: C => B, g: B => C): SplitMono[A, C] =
    SplitMono(get andThen g, f andThen reverseGet)

  /** SplitMono is an invariant functor over B. */
  def imapB[C](f: A => C, g: C => A): SplitMono[C, B] =
    SplitMono(g andThen get, reverseGet andThen f)

  /**
   * reverseGet and get, yielding a normalized formatted value. Subsequent reverseGet/get cycles are
   * idempotent.
   */
  def normalize(b: B): B =
    get(reverseGet(b))

}

object SplitMono {

  /** An Iso is trivially a SplitMono. */
  def fromIso[A, B](p: Iso[A, B]): SplitMono[A, B] =
    SplitMono(p.get, p.reverseGet)

  /** SplitMono forms a category. */
  implicit def SplitMonoCategory: Category[SplitMono] =
    new Category[SplitMono] {
      def id[A] = SplitMono(identity, identity)
      def compose[A, B, C](f: SplitMono[B, C], g: SplitMono[A, B]): SplitMono[A, C] = g ^<-! f
    }

}
