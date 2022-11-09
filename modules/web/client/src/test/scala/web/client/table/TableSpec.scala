// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client.table

import cats.tests.CatsSuite
import cats.kernel.laws.discipline.EqTests
import monocle.law.discipline.LensTests

final class TableSpec extends CatsSuite with TableArbitraries {
  checkAll("Eq[UserModified]", EqTests[UserModified].eqv)
  checkAll("Eq[FixedColumnWidth]", EqTests[FixedColumnWidth].eqv)
  checkAll("Eq[VariableColumnWidth]", EqTests[VariableColumnWidth].eqv)
  checkAll("Eq[ColumnWidth]", EqTests[ColumnWidth].eqv)
  checkAll("Eq[ColumnMeta[Int]]", EqTests[ColumnMeta[Int]].eqv)
  checkAll("Eq[TableState[Int]]", EqTests[TableState[Int]].eqv)
  checkAll("Lens[TableState[A], UserModified]", LensTests(TableState.userModified[Int]))
  checkAll("Lens[TableState[A], NonEmptyList[ColumnMeta[A]]]", LensTests(TableState.columns[Int]))
}
