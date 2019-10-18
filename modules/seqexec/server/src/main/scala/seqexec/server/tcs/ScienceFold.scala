// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats._
import cats.implicits._
import gem.enum.LightSinkName
import seqexec.server.tcs.TcsController._

sealed trait ScienceFold extends Product with Serializable

object ScienceFold {
  case object Parked extends ScienceFold
  final case class Position(source: LightSource, sink: LightSinkName, port: Int) extends ScienceFold

  implicit val positionEq: Eq[Position] = Eq.by(x => (x.source, x.sink, x.port))

  implicit val eq: Eq[ScienceFold] = Eq.instance{
    case (Parked, Parked)           => true
    case (a: Position, b: Position) => a === b
    case _                          => false
  }
}
