// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client.table

import cats.Eq
import cats.syntax.all._
import cats.data.NonEmptyList
import japgolly.scalajs.react.raw.JsNumber
import org.scalacheck._
import org.scalacheck.Arbitrary._
import react.common.syntax._
import scala.annotation.nowarn

trait TableArbitraries {
  implicit val arbUserModified: Arbitrary[UserModified] = Arbitrary {
    Gen.oneOf(IsModified, NotModified)
  }

  implicit val userModifiedCogen: Cogen[UserModified] =
    Cogen[String].contramap(_.productPrefix)

  val genFixedColumnWidth: Gen[FixedColumnWidth] =
    Gen.posNum[Double].map(FixedColumnWidth.apply)

  implicit val fixedColumnWidthArb: Arbitrary[FixedColumnWidth] =
    Arbitrary(genFixedColumnWidth)

  implicit val fixedColumnWidthCogen: Cogen[FixedColumnWidth] =
    Cogen[Double].contramap(_.width)

  val genVariableColumnWidth: Gen[VariableColumnWidth] =
    for {
      w <- Gen.choose[Double](0, 1)
      m <- Gen.choose[Double](0, 1)
    } yield VariableColumnWidth.unsafeFromDouble(w, m)

  implicit val VariableColumnWidthArb: Arbitrary[VariableColumnWidth] =
    Arbitrary(genVariableColumnWidth)

  implicit val percentColumnWidthCogen: Cogen[VariableColumnWidth] =
    Cogen[(Double, Double)].contramap(x => (x.percentage, x.minWidth))

  implicit val arbColumnWidth: Arbitrary[ColumnWidth] = Arbitrary {
    Gen.oneOf(genFixedColumnWidth, genVariableColumnWidth)
  }

  implicit val columnWidthCogen: Cogen[ColumnWidth] =
    Cogen[Either[FixedColumnWidth, VariableColumnWidth]].contramap {
      case x: FixedColumnWidth    => x.asLeft
      case x: VariableColumnWidth => x.asRight
    }

  implicit val arbJsNumber: Arbitrary[JsNumber] = Arbitrary {
    // type JsNumber = Byte | Short | Int | Float | Double
    Gen.oneOf[JsNumber](arbitrary[Byte],
                        arbitrary[Short],
                        arbitrary[Int],
                        arbitrary[Float],
                        arbitrary[Double]
    )
  }

  @nowarn
  implicit val jsNumberCogen: Cogen[JsNumber] =
    Cogen[Double].contramap { x =>
      (x: Any) match {
        case y: Byte   => y.toDouble
        case y: Short  => y.toDouble
        case y: Int    => y.toDouble
        case y: Float  => y.toDouble
        case y: Double => y
      }
    }

  implicit def columnMetaArb[A: Arbitrary]: Arbitrary[ColumnMeta[A]] =
    Arbitrary {
      for {
        a <- arbitrary[A]
        n <- Gen.alphaStr
        l <- Gen.alphaStr
        v <- arbitrary[Boolean]
        w <- arbitrary[ColumnWidth]
      } yield ColumnMeta(a, n, l, v, w)
    }

  implicit def columnMetaCogen[A: Cogen]: Cogen[ColumnMeta[A]] =
    Cogen[(A, String, String, Boolean, ColumnWidth)].contramap(x =>
      (x.column, x.name, x.label, x.visible, x.width)
    )

  implicit def tableStateArb[A: Arbitrary: Eq]: Arbitrary[TableState[A]] =
    Arbitrary {
      for {
        u <- arbitrary[UserModified]
        s <- Gen.posNum[Int]
        c <- Gen.nonEmptyListOf[ColumnMeta[A]](arbitrary[ColumnMeta[A]])
      } yield TableState(u, s, NonEmptyList.fromListUnsafe(c))
    }

  implicit def tableStateCogen[A: Cogen]: Cogen[TableState[A]] =
    Cogen[(UserModified, Double, List[ColumnMeta[A]])].contramap(x =>
      (x.userModified, x.scrollPosition.toDouble, x.columns.toList)
    )

  implicit def columnMetaNelArb[A: Arbitrary: Eq]: Arbitrary[NonEmptyList[ColumnMeta[A]]] =
    Arbitrary {
      for {
        c <- Gen.nonEmptyListOf[ColumnMeta[A]](arbitrary[ColumnMeta[A]])
      } yield NonEmptyList.fromListUnsafe(c)
    }

  implicit def columnMetaNelCogen[A: Cogen]: Cogen[NonEmptyList[ColumnMeta[A]]] =
    Cogen[List[ColumnMeta[A]]].contramap(_.toList)
}

object TableArbitraries extends TableArbitraries
