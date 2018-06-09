// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.laws

import cats.{ Semigroup, Monoid, Group }

abstract class SemigroupHomomorphismLaws[A, B](f: A => B) {
  val A: Semigroup[A]
  val B: Semigroup[B]

  def combine(a: A, b: A): IsEq[B] =
    f(A.combine(a, b)) <-> B.combine(f(a), f(b))

}

abstract class MonoidHomomorphismLaws[A, B](f: A => B) extends SemigroupHomomorphismLaws[A, B](f) {
  override val A: Monoid[A]
  override val B: Monoid[B]

  def empty: IsEq[B] =
    f(A.empty) <-> B.empty

}

abstract class GroupHomomorphismLaws[A, B](f: A => B) extends MonoidHomomorphismLaws[A, B](f) {
  override val A: Group[A]
  override val B: Group[B]

  def inverse(a: A): IsEq[B] =
    f(A.inverse(a)) <-> B.inverse(f(a))

}