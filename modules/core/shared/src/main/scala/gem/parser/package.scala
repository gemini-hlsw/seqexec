// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

/**
 * Package of [[https://github.com/tpolecat/atto atto]] parsers, for parsing things in the science
 * model. The idea here is that parsers compose, so for internal use it's nice to have them together
 * and so we can share implementations. In end-user code we normally want to expose something
 * weaker like `String => Option[A]` that delegates to a parser, rather than exposing `Parser[A]`
 * directly. Each set of parsers is provided as a trait that can be extended and as a module whose
 * members can be imported (preferred).
 */
package object parser
