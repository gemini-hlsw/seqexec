// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client

// Produce a configuration string for Giapi.
trait GiapiConfig[T] {
  def configValue(t: T): String
}