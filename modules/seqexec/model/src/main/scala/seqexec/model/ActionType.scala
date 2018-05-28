// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import seqexec.model.Model.Resource

import cats.Eq
import cats.implicits._

sealed trait ActionType extends Product with Serializable

object ActionType {

  case object Observe extends ActionType
  // Used in tests
  case object Undefined extends ActionType
  final case class Configure(sys: Resource) extends ActionType

  implicit val equal: Eq[ActionType] = Eq.instance {
    case (_: Observe.type,   _: Observe.type)   => true
    case (_: Undefined.type, _: Undefined.type) => true
    case (Configure(a),         Configure(b))   => a === b
    case _                                      => false
  }

}
