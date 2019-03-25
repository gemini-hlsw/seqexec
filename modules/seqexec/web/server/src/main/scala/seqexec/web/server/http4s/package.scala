// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server

import cats.implicits._
import gem.Observation
import seqexec.model.QueueId
import seqexec.model.Observer
import seqexec.model.Operator
import seqexec.model.enum.Instrument
import seqexec.model.enum.Resource
import seqexec.model.ClientId

trait Var {
  object ObsIdVar {
    def unapply(str: String): Option[Observation.Id] =
      Observation.Id.fromString(str)
  }

  object ObserverVar {
    def unapply(str: String): Option[Observer] =
      Observer(str).some
  }

  object OperatorVar {
    def unapply(str: String): Option[Operator] =
      Operator(str).some
  }

  object InstrumentVar {
    def unapply(str: String): Option[Instrument] =
      Instrument.all.find(_.show === str)
  }

  object ClientIDVar {
    def unapply(str: String): Option[ClientId] =
      Either.catchNonFatal(ClientId(java.util.UUID.fromString(str))).toOption
  }

  object QueueIdVar {
    def unapply(str: String): Option[QueueId] =
      Either.catchNonFatal(QueueId(java.util.UUID.fromString(str))).toOption
  }

  object PosIntVar {
    def unapply(str: String): Option[Int] =
      Either.catchNonFatal(str.toInt).toOption.filter(_ >= 0)
  }

  object ResourceVar {
    def unapply(str: String): Option[Resource] =
      Instrument.allResources.find(_.show === str)
  }

  object BooleanVar {
    def unapply(str: String): Option[Boolean] =
      str.toLowerCase match {
        case "true"  => Some(true)
        case "false" => Some(false)
        case _       => None
      }
  }

  object IntVar {
    def unapply(str: String): Option[Int] =
      Either.catchNonFatal(str.toInt).toOption
  }
}
package object http4s extends Var
