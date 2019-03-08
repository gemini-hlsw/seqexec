// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.implicits._
import mouse.string._
import gem.enum.LightSinkName
import seqexec.server.EpicsCodex.{DecodeEpicsValue, EncodeEpicsValue}
import seqexec.server.tcs.TcsController.LightSource
import seqexec.server.tcs.TcsControllerEpics.ScienceFold.{Parked, Position}
import seqexec.server.tcs.TcsControllerEpics.ScienceFold

// Decoding and encoding the science fold position require some common definitions, therefore I
// put them inside an object
private[server] object ScienceFoldPositionCodex {

  import LightSource._

  private val AO_PREFIX = "ao2"
  private val GCAL_PREFIX = "gcal2"
  private val PARK_POS = "park-pos"

  private def findSinkInSFName(str: String): Option[LightSinkName] =
    LightSinkName.all.find(i => str.startsWith(i.name))

  private def findPortInSFName(str: String): Option[Int] = str.parseIntOption

  implicit val decodeScienceFold: DecodeEpicsValue[String, Option[ScienceFold]] = DecodeEpicsValue(
    (t: String) => if (t.startsWith(PARK_POS)) Parked.some
    else if (t.startsWith(AO_PREFIX)) for {
      sink <- findSinkInSFName(t.substring(AO_PREFIX.length))
      port <- findPortInSFName(t.substring(AO_PREFIX.length + sink.name.length))
    } yield Position(AO, sink, port)
    else if (t.startsWith(GCAL_PREFIX)) for {
      sink <- findSinkInSFName(t.substring(GCAL_PREFIX.length))
      port <- findPortInSFName(t.substring(GCAL_PREFIX.length + sink.name.length))
    } yield Position(GCAL, sink, port)
    else for {
      sink <- findSinkInSFName(t)
      port <- findPortInSFName(t.substring(sink.name.length))
    } yield Position(Sky, sink, port)
  )

  implicit val encodeScienceFold: EncodeEpicsValue[Position, String] = EncodeEpicsValue((a: Position) => {
    val instAGName = a.sink.name + a.port.toString

    a.source match {
      case Sky => instAGName
      case AO => AO_PREFIX + instAGName
      case GCAL => GCAL_PREFIX + instAGName
    }
  })

}
