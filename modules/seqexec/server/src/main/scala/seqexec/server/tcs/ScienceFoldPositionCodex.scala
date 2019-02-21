// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.effect.IO
import cats.implicits._
import gem.enum.LightSinkName
import seqexec.server.EpicsCodex.{DecodeEpicsValue, EncodeEpicsValue}
import seqexec.server.tcs.TcsController.{LightSource, ScienceFoldPosition}

// Decoding and encoding the science fold position require some common definitions, therefore I
// put them inside an object
private[server] object ScienceFoldPositionCodex {

  import LightSource._
  import ScienceFoldPosition._

  private val AO_PREFIX = "ao2"
  private val GCAL_PREFIX = "gcal2"
  private val PARK_POS = "park-pos"

  val BottomPort:Int = 1

  def portFromSinkName(n: LightSinkName): IO[Option[Int]] = {
    val InvalidPort = 0
    (n match {
      case LightSinkName.Gmos |
           LightSinkName.Gmos_Ifu => TcsEpics.instance.gmosPort
      case LightSinkName.Niri_f6 |
           LightSinkName.Niri_f14 |
           LightSinkName.Niri_f32 => TcsEpics.instance.niriPort
      case LightSinkName.Nifs     => TcsEpics.instance.nifsPort
      case LightSinkName.Gnirs    => TcsEpics.instance.gnirsPort
      case LightSinkName.F2       => TcsEpics.instance.f2Port
      case LightSinkName.Gpi      => TcsEpics.instance.gpiPort
      case LightSinkName.Ghost    => TcsEpics.instance.ghostPort
      case LightSinkName.Gsaoi    => TcsEpics.instance.gsaoiPort
      case LightSinkName.Ac |
           LightSinkName.Hr       => IO(BottomPort.some)
      case LightSinkName.Phoenix |
           LightSinkName.Visitor  => IO(None)
    }).map(_.filterNot(_ === InvalidPort))
  }

  private def findSinkInSFName(str: String): Option[LightSinkName] =
    LightSinkName.all.find(i => str.startsWith(i.name))

  implicit val decodeScienceFoldPosition: DecodeEpicsValue[String, Option[ScienceFoldPosition]] = DecodeEpicsValue(
    (t: String) => if (t.startsWith(PARK_POS)) Parked.some
    else if (t.startsWith(AO_PREFIX))
      findSinkInSFName(t.substring(AO_PREFIX.length)).map(Position(AO, _))
    else if (t.startsWith(GCAL_PREFIX))
      findSinkInSFName(t.substring(GCAL_PREFIX.length)).map(Position(GCAL, _))
    else findSinkInSFName(t).map(Position(Sky, _))
  )

  def encodeScienceFoldPosition(port: Int): EncodeEpicsValue[Position, String] = EncodeEpicsValue((a: Position) => {
    val instAGName = a.sink.name + port.toString

    a.source match {
      case Sky => instAGName
      case AO => AO_PREFIX + instAGName
      case GCAL => GCAL_PREFIX + instAGName
    }
  })
}
