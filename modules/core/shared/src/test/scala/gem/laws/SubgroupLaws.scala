// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.laws

// A is a subgroup of B if covariant widening is a group homomorphism.
abstract class SubgroupLaws[A, B](f: A <:< B) extends GroupHomomorphismLaws[A, B](f)
