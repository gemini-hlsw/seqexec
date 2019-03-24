// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.common

import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.{FoldableTests, FunctorTests, TraverseTests}
import cats.tests.CatsSuite

/**
  * Tests the Monocle Lenses for Seqexec Events
  */
@SuppressWarnings(Array("org.wartremover.warts.ToString"))
final class FixedLengthBufferSpec extends CatsSuite {
  import ArbitrariesWebCommon.arbFixedLengthBuffer
  import ArbitrariesWebCommon.fixedLengthBufferCogen

  checkAll("Eq[FixedLengthBuffer]", EqTests[FixedLengthBuffer[Int]].eqv)
  checkAll("Functor[FixedLengthBuffer]", FunctorTests[FixedLengthBuffer].functor)
  checkAll("Foldable[FixedLengthBuffer]", FoldableTests[FixedLengthBuffer].foldable[Int, Int])
  checkAll("Traversable[FixedLengthBuffer]", TraverseTests[FixedLengthBuffer].traverse[Int, Int, Int, Int, Option, Option])
}
