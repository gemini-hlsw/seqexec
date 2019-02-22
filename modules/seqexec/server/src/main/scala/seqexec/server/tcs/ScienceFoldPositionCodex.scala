// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.implicits._
import mouse.boolean._
import gem.enum.LightSinkName
import seqexec.server.EpicsCodex.{DecodeEpicsValue, EncodeEpicsValue}
import seqexec.server.tcs.TcsController.{LightSource, ScienceFoldPosition}
import seqexec.server.tcs.TcsControllerEpics.InstrumentPorts

// Decoding and encoding the science fold position require some common definitions, therefore I
// put them inside an object
private[server] object ScienceFoldPositionCodex {

  import LightSource._
  import ScienceFoldPosition._

  private val AO_PREFIX = "ao2"
  private val GCAL_PREFIX = "gcal2"
  private val PARK_POS = "park-pos"

  val BottomPort: Int = 1
  val InvalidPort: Int = 0

  def portFromSinkName(ports: InstrumentPorts)(n: LightSinkName): Option[Int] = {
    val port = n match {
      case LightSinkName.Gmos |
           LightSinkName.Gmos_Ifu => ports.gmosPort
      case LightSinkName.Niri_f6 |
           LightSinkName.Niri_f14 |
           LightSinkName.Niri_f32 => ports.niriPort
      case LightSinkName.Nifs     => ports.nifsPort
      case LightSinkName.Gnirs    => ports.gnirsPort
      case LightSinkName.F2       => ports.flamingos2Port
      case LightSinkName.Gpi      => ports.gpiPort
      case LightSinkName.Ghost    => ports.ghostPort
      case LightSinkName.Gsaoi    => ports.gsaoiPort
      case LightSinkName.Ac |
           LightSinkName.Hr       => BottomPort
      case LightSinkName.Phoenix |
           LightSinkName.Visitor  => InvalidPort
    }
    (port =!= InvalidPort).option(port)
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
