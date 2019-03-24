// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql

/** Minimal classes to refer to other enums by id. This is required to construct
 *  enums refering to others like Gpi Observing Mode. */
object EnumRefs {
  final case class EnumRef[T <: Symbol](tag: String)
  final case class LazyEnumRef[T <: Symbol](tag: String)
}
