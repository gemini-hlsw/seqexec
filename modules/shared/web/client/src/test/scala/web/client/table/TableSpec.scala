// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client.table

import cats.Eq
import cats.tests.CatsSuite
import cats.data.NonEmptyList
import cats.kernel.laws.discipline.EqTests
import japgolly.scalajs.react.raw.JsNumber
import monocle.law.discipline.LensTests
import org.scalacheck._
import org.scalacheck.Arbitrary._
import web.client.utils._

final class TableSpec extends CatsSuite {
  implicit val arbUserModified: Arbitrary[UserModified] = Arbitrary {
    Gen.oneOf(IsModified, NotModified)
  }

  implicit val userModifiedCogen: Cogen[UserModified] =
    Cogen[String].contramap(_.productPrefix)

  val genFixedColumnWidth: Gen[FixedColumnWidth] =
    Gen.posNum[Int].map(FixedColumnWidth.apply)

  implicit val fixedColumnWidthCogen: Cogen[FixedColumnWidth] =
    Cogen[Int].contramap(_.width)

  val genPercentageColumnWidth: Gen[PercentageColumnWidth] =
    Gen.choose[Double](0, 1).map(PercentageColumnWidth.apply)

  implicit val percentColumnWidthCogen: Cogen[PercentageColumnWidth] =
    Cogen[Double].contramap(_.percentage)

  implicit val arbColumnWidth: Arbitrary[ColumnWidth] = Arbitrary {
    Gen.oneOf(genFixedColumnWidth, genPercentageColumnWidth)
  }

  implicit val columnWidthCogen: Cogen[ColumnWidth] =
    Cogen[Either[FixedColumnWidth, PercentageColumnWidth]].contramap {
      case x: FixedColumnWidth      => x.asLeft
      case x: PercentageColumnWidth => x.asRight
    }

  implicit val arbJsNumber: Arbitrary[JsNumber] = Arbitrary {
    // type JsNumber = Byte | Short | Int | Float | Double
    Gen.oneOf[JsNumber](arbitrary[Byte], arbitrary[Short], arbitrary[Int], arbitrary[Float], arbitrary[Double])
  }

  implicit val jsNumberCogen: Cogen[JsNumber] =
    Cogen[Double].contramap {x => (x: Any) match {
      case y: Byte   => y.toDouble
      case y: Short  => y.toDouble
      case y: Int    => y.toDouble
      case y: Float  => y.toDouble
      case y: Double => y
    }}

  implicit def columnMetaArb[A: Arbitrary]: Arbitrary[ColumnMeta[A]] = Arbitrary {
    for {
      a <- arbitrary[A]
      n <- arbitrary[String]
      l <- arbitrary[String]
      v <- arbitrary[Boolean]
      w <- arbitrary[ColumnWidth]
    } yield ColumnMeta(a, n, l, v, w)
  }

  implicit def columnMetaCogen[A: Cogen]: Cogen[ColumnMeta[A]] =
    Cogen[(A, String, String, Boolean, ColumnWidth)].contramap(x => (x.column, x.name, x.label, x.visible, x.width))

  implicit def tableStateArb[A: Arbitrary: Eq]: Arbitrary[TableState[A]] = Arbitrary {
    for {
      u <- arbitrary[UserModified]
      s <- arbitrary[JsNumber]
      c <- Gen.nonEmptyListOf[ColumnMeta[A]](arbitrary[ColumnMeta[A]])
    } yield TableState(u, s, NonEmptyList.fromListUnsafe(c))
  }

  implicit def tableStateCogen[A: Cogen]: Cogen[TableState[A]] =
    Cogen[(UserModified, Double, List[ColumnMeta[A]])].contramap(x => (x.userModified, x.scrollPosition.toDouble, x.columns.toList))

  implicit def columnMetaNonEmptyListArb[A: Arbitrary: Eq]: Arbitrary[NonEmptyList[ColumnMeta[A]]] = Arbitrary {
    for {
      c <- Gen.nonEmptyListOf[ColumnMeta[A]](arbitrary[ColumnMeta[A]])
    } yield NonEmptyList.fromListUnsafe(c)
  }

  implicit def columnMetaNelCogen[A: Cogen]: Cogen[NonEmptyList[ColumnMeta[A]]] =
    Cogen[List[ColumnMeta[A]]].contramap(_.toList)

  checkAll("Eq[UserModified]", EqTests[UserModified].eqv)
  checkAll("Eq[ColumnMeta[Int]]", EqTests[ColumnMeta[Int]].eqv)
  checkAll("Eq[TableState[Int]]", EqTests[TableState[Int]].eqv)
  checkAll("Lens[TableState[A], UserModified]", LensTests(TableState.userModified[Int]))
  checkAll("Lens[TableState[A], NonEmptyList[ColumnMeta[A]]]", LensTests(TableState.columns[Int]))
}
