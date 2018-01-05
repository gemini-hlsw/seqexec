// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.util

import cats.arrow._
import monocle.{ Iso, Prism }

/**
 * A normalizing optic, isomorphic to Prism but with different laws, specifically `getOption` 
 * (here called `parse) need not be injective; i.e., distinct inputs may have the same parse
 * result, which combined with a subsequent `reverseGet` (here called `format`) yield a normalized
 * form for A. Composition with stronger optics (`Prism` and `Iso`) yields another `Format`.
 */
final case class Format[A, B](parse: A => Option[B], format: B => A) {

  /** Compose with another Format. */
  def composeFormat[C](f: Format[B, C]): Format[A, C] =
    Format(parse(_).flatMap(f.parse), format compose f.format)

  /** Compose with another Format. */
  def composePrism[C](f: Prism[B, C]): Format[A, C] =
    Format(parse(_).flatMap(f.getOption), format compose f.reverseGet)

  /** Compose with another Format. */
  def composeIso[C](f: Iso[B, C]): Format[A, C] =
    Format(parse(_).map(f.get), format compose f.reverseGet)

  /** Alias to composeFormat. */
  def ^<-*[C](f: Format[B, C]): Format[A, C] =
    composeFormat(f)

  /** Alias to composePrism. */
  def ^<-?[C](f: Prism[B, C]): Format[A, C] =
    composePrism(f)

  /** Alias to composeIso. */
  def ^<->[C](f: Iso[B, C]): Format[A, C] =
    composeIso(f)

  /** Format is an invariant functor over A. */
  def imapA[C](f: C => B, g: B => C): Format[A, C] =
    Format(parse(_).map(g), f andThen format)

  /** Format is an invariant functor over B. */
  def imapB[C](f: A => C, g: C => A): Format[C, B] =
    Format(g andThen parse, format andThen f)

  /**
   * Parse and format, yielding a normalized formatted value. Subsequent parse/format cycles are
   * idempotent.
   */
  def normalize(b: A): Option[A] =
    parse(b).map(format)

  /** If we can format a Product as a String we can implement a tagged toString like "Foo(stuff)". */
  def productToString(b: B)(
    implicit as: A =:= String,
             bp: B <:< Product
  ): String =
    taggedToString(b.productPrefix, b)

  /**
   * If we provide a tag like "Foo" and format as a String we can implement a nice toString like
   * "Foo(stuff)".
   */
  def taggedToString(tag: String, b: B)(
    implicit as: A =:= String
  ): String =
    new StringBuilder(tag)
      .append('(')
      .append(as(format(b)))
      .append(')')
      .toString

}

object Format {

  /** A Prism is trivially a Format. */
  def fromPrism[A, B](p: Prism[A, B]): Format[A, B] =
    Format(p.getOption, p.reverseGet)

  /** An Iso is trivially a Format. */
  def fromIso[A, B](p: Iso[A, B]): Format[A, B] =
    Format(a => Some(p.get(a)), p.reverseGet)
    
  /** Format forms a category. */
  implicit def FormatCategory: Category[Format] =
    new Category[Format] {
      def id[A] = Format(Some(_), identity)
      def compose[A, B, C](f: Format[B, C], g: Format[A, B]): Format[A, C] = g ^<-* f
    }

}