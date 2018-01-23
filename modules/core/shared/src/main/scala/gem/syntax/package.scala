// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

/**
 * Syntax classes for extension methods, organized à la cats. Each syntax class has an associated
 * conversion trait and module that extends it; and the `all` module which extends all
 * conversions traits.
 */
package object syntax {
  object all extends ToParserOps
                with ToPrismOps
                with ToStringOps
                with ToInstantOps
                with ToDurationOps
                with ToTreeSetCompanionOps
}
