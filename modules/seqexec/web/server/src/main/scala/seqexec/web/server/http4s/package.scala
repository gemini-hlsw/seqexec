// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server

import cats.implicits._
import gem.Observation
import seqexec.model.{ Observer, Operator }
import seqexec.model.enum.Instrument
import seqexec.model.ClientID

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
    def unapply(str: String): Option[ClientID] =
      Either.catchNonFatal(java.util.UUID.fromString(str)).toOption
  }

  object PosIntVar {
    def unapply(str: String): Option[Int] =
      Either.catchNonFatal(str.toInt).toOption.filter(_ >= 0)
  }

  object BooleanVar {
    def unapply(str: String): Option[Boolean] =
      str.toLowerCase match {
        case "true"  => Some(true)
        case "false" => Some(false)
        case _       => None
      }
  }
}
package object http4s extends Var
