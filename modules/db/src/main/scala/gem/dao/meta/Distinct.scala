// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import cats.data.NonEmptyList
import doobie._
import doobie.enum.JdbcType.{ Distinct => JdbcDistinct, _ }

/**
 * Constructor for a Meta instances with an underlying types that are reported by JDBC as
 * type Distinct, as happens when a column has a check constraint. By using a data type with
 * a Distinct Meta instance we can satisfy the query checker.
 */
object Distinct {

  def integer(name: String): Meta[Int] =
    Meta.Advanced.many(
      NonEmptyList.of(JdbcDistinct, Integer),
      NonEmptyList.of(name),
      _ getInt _,
      _.setInt(_, _),
      _.updateInt(_, _)
    )

  def long(name: String): Meta[Long] =
    Meta.Advanced.many(
      NonEmptyList.of(JdbcDistinct, BigInt),
      NonEmptyList.of(name),
      _ getLong _,
      _.setLong(_, _),
      _.updateLong(_, _)
    )

  def short(name: String): Meta[Short] =
    Meta.Advanced.many(
      NonEmptyList.of(JdbcDistinct, SmallInt),
      NonEmptyList.of(name),
      _ getShort _,
      _.setShort(_, _),
      _.updateShort(_, _)
    )

  def string(name: String): Meta[String] =
    Meta.Advanced.many(
      NonEmptyList.of(JdbcDistinct, VarChar),
      NonEmptyList.of(name),
      _ getString _,
      _.setString(_, _),
      _.updateString(_, _)
    )

}
