// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server

import cats.syntax.all._
import org.http4s.QueryParamDecoder
import org.http4s.dsl.impl.OptionalQueryParamDecoderMatcher
import seqexec.model.ClientId
import seqexec.model.Observation
import seqexec.model.Observer
import seqexec.model.Operator
import seqexec.model.QueueId
import seqexec.model.enum.Instrument
import seqexec.model.enum.Resource
import seqexec.model.enum.RunOverride

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

trait QueryParams {
  implicit val RunOverrideQueryParamDecoder: QueryParamDecoder[RunOverride] =
    QueryParamDecoder[Boolean].map {
      case true => RunOverride.Override
      case _    => RunOverride.Default
    }

}

package object http4s extends Var with QueryParams {
  object OptionalRunOverride
      extends OptionalQueryParamDecoderMatcher[RunOverride]("overrideTargetCheck")
}
