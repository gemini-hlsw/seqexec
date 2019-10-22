// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.implicits._
import seqexec.model.enum.Resource

sealed trait ActionType extends Product with Serializable

object ActionType {

        case object Observe                  extends ActionType
        case object Undefined                extends ActionType // Used in tests
  final case class  Configure(sys: Resource) extends ActionType

  implicit val equal: Eq[ActionType] =
    Eq.instance {
      case (Observe, Observe)           => true
      case (Undefined, Undefined)       => true
      case (Configure(a), Configure(b)) => a === b
      case _                            => false
    }

}
