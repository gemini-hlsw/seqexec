// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.Eq
import cats.implicits._
import gem.Observation
// import seqexec.model.ClientId

sealed trait SingleActionOp extends Product with Serializable {
  val sid: Observation.Id
  val resource: Resource
}

object SingleActionOp {
  final case class Started(sid: Observation.Id, resource: Resource)
    extends SingleActionOp
  final case class Completed(sid: Observation.Id, resource: Resource)
    extends SingleActionOp
  final case class Error(sid: Observation.Id, resource: Resource)
    extends SingleActionOp

  implicit val equal: Eq[SingleActionOp] = Eq.instance {
    case (Started(a, c), Started(b, d))     => a === b && c === d
    case (Completed(a, c), Completed(b, d)) => a === b && c === d
    case (Error(a, c), Error(b, d))         => a === b && c === d
    case _                                  => false
  }
}