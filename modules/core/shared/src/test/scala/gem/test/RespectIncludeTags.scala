// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.test

import org.scalatest._


/** Reverses the meaning of Scala Test test filters.  The TLDR version of what
  * this does is:
  *
  * - Allows explicit include filters to override exclude filters
  * - Explicit include filters don't exclude anything
  *
  * This makes it possible to specify Tags and apply them to test cases that
  * usually should not be run but that can be run when desired.
  *
  *
  * Setting up Tests Excluded by Default
  *
  * 1. First chose or create a new Tag (see `gem.test.Tags`).  For example,
  *    `gem.test.Tags.RequiresNetwork` for tests that require an external
  *    resource.
  *
  * 2. Make sure the build contains a setting to exclude the tag.  For example
  *
  *        testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-l", "gem.test.Tags.RequiresNetwork") // by default, ignore network tests
  *
  * 3. Now, to turn on tests that are normally excluded use
  *
  *        `testOnly testName -- -n gem.test.Tags.RequiresNetwork`
  *
  *    Unfortunately this doesn't work with the `test` command itself.  For that
  *    you must update the setting which can be done on the SBT command line with:
  *
  *        set testOptions in ThisBuild += Tests.Argument(TestFrameworks.ScalaTest, "-n", "gem.test.Tags.RequiresNetwork")
  *
  *
  * Background
  *
  * ScalaTest provides the concept of test tags that can be used to control
  * which tests are executed at runtime.  In particular, two filters are
  * available, an "include" filter and an "exclude" filter.  The include filter
  * excludes everything except tests tagged with the given tags, provided they
  * are not also mentioned in the exclude filter.  The exclude filter just lists
  * tags whose associated tests should not be executed.  Using the SB `testOnly`
  * command, include tags can be specified with `-n` and exclude tags with `-l`
  * (that's lowercase L).  See
  *
  * http://www.scalatest.org/user_guide/using_scalatest_with_sbt
  *
  * for more information.
  *
  */
trait RespectIncludeTags extends Suite {

  override protected def runTests(testName: Option[String], args: Args): Status = {

    val filter     = args.filter
    val isExcluded = filter.tagsToExclude
    val isIncluded = filter.tagsToInclude.getOrElse(Set.empty)
    val exclude    = isIncluded.foldLeft(isExcluded)(_ - _)
    val newFilter  = Filter(None, exclude, filter.excludeNestedSuites, filter.dynaTags)

    super.runTests(testName, args.copy(filter = newFilter))
  }

}
