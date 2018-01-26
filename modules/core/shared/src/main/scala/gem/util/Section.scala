// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.util

import cats.arrow.Category
import monocle.Iso

/**
 * A normalizing optic, isomorphic to Iso but with different laws, specifically `get`
 * need not be injective; i.e., distinct inputs may have the same `get` result, which combined
 * with a subsequent `reverseGet` yield a normalized form for A.
 */
@SuppressWarnings(Array("org.wartremover.warts.Null"))
final case class Section[A, B](get: A => B, reverseGet: B => A) {

  /** Compose with another Section. */
  def composeSection[C](f: Section[B, C]): Section[A, C] =
    Section(a => f.get(get(a)), reverseGet compose f.reverseGet)

  /** Compose with an Iso. */
  def composeIso[C](f: Iso[B, C]): Section[A, C] =
    Section(a => f.get(get(a)), reverseGet compose f.reverseGet)

  /** Alias to composeSection. */
  def ^<-![C](f: Section[B, C]): Section[A, C] =
    composeSection(f)

  /** Alias to composeIso. */
  def ^<->[C](f: Iso[B, C]): Section[A, C] =
    composeIso(f)

  /** Section is an invariant functor over A. */
  def imapA[C](f: C => B, g: B => C): Section[A, C] =
    Section(get andThen g, f andThen reverseGet)

  /** Section is an invariant functor over B. */
  def imapB[C](f: A => C, g: C => A): Section[C, B] =
    Section(g andThen get, reverseGet andThen f)

  /**
   * get and reverseGet, yielding a normalized formatted value. Subsequent get/reverseGet cycles are
   * idempotent.
   */
  def normalize(b: A): A =
    reverseGet(get(b))

  /** If we can reverseGet a Product as a String we can implement a tagged toString like "Foo(stuff)". */
  def productToString(b: B)(
    implicit as: A =:= String,
             bp: B <:< Product
  ): String =
    taggedToString(b.productPrefix, b)

  /**
   * If we provide a tag like "Foo" and reverseGet as a String we can implement a nice toString like
   * "Foo(stuff)".
   */
  def taggedToString(tag: String, b: B)(
    implicit as: A =:= String
  ): String =
    new StringBuilder(tag)
      .append('(')
      .append(as(reverseGet(b)))
      .append(')')
      .toString

}

object Section {

  /** An Iso is trivially a Section. */
  def fromIso[A, B](p: Iso[A, B]): Section[A, B] =
    Section(p.get, p.reverseGet)

  /** Section forms a category. */
  implicit def SectionCategory: Category[Section] =
    new Category[Section] {
      def id[A] = Section(identity, identity)
      def compose[A, B, C](f: Section[B, C], g: Section[A, B]): Section[A, C] = g ^<-! f
    }

}