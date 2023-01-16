// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client.syntax

import giapi.client.GiapiConfig

final class GiapiConfigOps[A](val a: A) extends AnyVal {
  def configValue(implicit c: GiapiConfig[A]): String =
    c.configValue(a)
}

trait ToGiapiConfigOps {
  implicit def ToGiapiConfigOps[A](value: A): GiapiConfigOps[A] =
    new GiapiConfigOps(value)
}

object giapiconfig extends ToGiapiConfigOps
