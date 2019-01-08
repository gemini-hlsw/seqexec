// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client

import cats.Show

// Produce a configuration string for Giapi.
trait GiapiConfig[T] {
  def configuration(t: T): String
}

object GiapiConfig {
  def apply[A](implicit instance: GiapiConfig[A]): GiapiConfig[A] = instance

  implicit class GiapiConfigOps[A](value: A) {
    def configuration(implicit w: GiapiConfig[A]): String =
      w.configuration(value)
  }

  def fromShow[A: Show]: GiapiConfig[A] = Show[A].show(_)

  implicit val stringConfig: GiapiConfig[String] = t => t
  implicit val intConfig: GiapiConfig[Int] = _.toString
  implicit val doubleConfig: GiapiConfig[Double] = _.toString
}
