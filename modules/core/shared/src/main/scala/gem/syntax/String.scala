// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.syntax

final class StringOps(val self: String) extends AnyVal {

  private def parse[A](f: String => A): Option[A] =
    try Some(f(self)) catch { case _: IllegalArgumentException => None }

  def parseShortOption:   Option[Short]   = parse(_.toShort)
  def parseIntOption:     Option[Int]     = parse(_.toInt)
  def parseLongOption:    Option[Long]    = parse(_.toLong)
  def parseDoubleOption:  Option[Double]  = parse(_.toDouble)
  def parseBooleanOption: Option[Boolean] = parse(_.toBoolean)
  def parseBigDecimalOption: Option[BigDecimal] = parse(BigDecimal(_)) 

}

trait ToStringOps {
  implicit def ToStringOps[A](p: String): StringOps = new StringOps(p)
}

object string extends ToStringOps
