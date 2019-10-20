// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.implicits._
import atto._, Atto._
import gem.enum.LightSinkName
import seqexec.server.EpicsCodex.{DecodeEpicsValue, EncodeEpicsValue}
import seqexec.server.tcs.TcsController.LightSource._
import seqexec.server.tcs.TcsController.LightSource
import seqexec.server.tcs.ScienceFold.{Parked, Position}

// Decoding and encoding the science fold position require some common definitions, therefore I
// put them inside an object
private[server] trait ScienceFoldPositionCodex {

  private val AO_PREFIX = "ao2"
  private val GCAL_PREFIX = "gcal2"
  private val PARK_POS = "park-pos"

  val lightSink: Parser[LightSinkName] = LightSinkName.all.foldMap(x => string(x.name).as(x))

  def prefixed(p: String, s: LightSource): Parser[ScienceFold] =
    (string(p) ~> lightSink ~ int).map { case (ls, port) => Position(s, ls, port) }

  val park: Parser[ScienceFold] =
    (string(PARK_POS) <~ many(anyChar)).as(Parked)

  implicit val decodeScienceFold: DecodeEpicsValue[String, Option[ScienceFold]] = DecodeEpicsValue(
    (t: String) =>
      (park | prefixed(AO_PREFIX, AO) | prefixed(GCAL_PREFIX, GCAL) | prefixed("", Sky)).parseOnly(t).option  )

  implicit val encodeScienceFold: EncodeEpicsValue[Position, String] = EncodeEpicsValue((a: Position) => {
    val instAGName = a.sink.name + a.port.toString

    a.source match {
      case Sky  => instAGName
      case AO   => AO_PREFIX + instAGName
      case GCAL => GCAL_PREFIX + instAGName
    }
  })

}

object ScienceFoldPositionCodex extends ScienceFoldPositionCodex
