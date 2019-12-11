// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.common

import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.{FoldableTests, FunctorTests, TraverseTests}
import cats.tests.CatsSuite
import seqexec.common.ArbitrariesCommon.arbFixedLengthBuffer
import seqexec.common.ArbitrariesCommon.fixedLengthBufferCogen

/**
  * Tests the Monocle Lenses for Seqexec Events
  */
final class FixedLengthBufferSpec extends CatsSuite {

  checkAll("Eq[FixedLengthBuffer]", EqTests[FixedLengthBuffer[Int]].eqv)
  checkAll("Functor[FixedLengthBuffer]", FunctorTests[FixedLengthBuffer].functor[Int, Int, Int])
  checkAll("Foldable[FixedLengthBuffer]", FoldableTests[FixedLengthBuffer].foldable[Int, Int])
  checkAll("Traversable[FixedLengthBuffer]", TraverseTests[FixedLengthBuffer].traverse[Int, Int, Int, Int, Option, Option])
}
