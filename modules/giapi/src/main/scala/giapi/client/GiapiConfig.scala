// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client

// Produce a configuration string for Giapi.
trait GiapiConfig[T] {
  def configValue(t: T): String
}

object GiapiConfig {
  implicit val stringConfig: GiapiConfig[String] = t => t
  implicit val intConfig: GiapiConfig[Int]       = _.toString
  implicit val doubleConfig: GiapiConfig[Double] = d => f"$d%1.6f"
  implicit val floatConfig: GiapiConfig[Float]   = d => f"$d%1.6f"

  @inline
  def apply[A](implicit instance: GiapiConfig[A]): GiapiConfig[A] = instance

  def instance[A](f: A => String): GiapiConfig[A] = new GiapiConfig[A] {
    def configValue(t: A) = f(t)
  }
}
