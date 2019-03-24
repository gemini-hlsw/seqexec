// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.test

import org.scalatest.Tag

/** Scalatest tags.
  */
object Tags {

  /** Tag tests that require network and external services access. */
  object RequiresNetwork extends Tag("gem.test.Tags.RequiresNetwork")

  /** Tag tests that are unusually slow. */
  object Slow extends Tag("gem.test.Tags.Slow")
}
