// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client.syntax

import cats.Show
import giapi.client.GiapiConfig

final class GiapiConfigOps[A](val a: A) extends AnyVal {
  def configValue(implicit c: GiapiConfig[A]): String =
    c.configValue(a)
}

trait ToGiapiCofigOps {
  implicit def ToGiapiConfigOps[A](value: A): GiapiConfigOps[A] = new GiapiConfigOps(value)
}

object giapiconfig extends ToGiapiCofigOps {
  implicit val stringConfig: GiapiConfig[String] = t => t
  implicit val intConfig: GiapiConfig[Int] = _.toString
  implicit val doubleConfig: GiapiConfig[Double] = d => f"$d%1.6f"
  def fromShow[A: Show]: GiapiConfig[A] = Show[A].show(_)
  def apply[A](implicit instance: GiapiConfig[A]): GiapiConfig[A] = instance
}