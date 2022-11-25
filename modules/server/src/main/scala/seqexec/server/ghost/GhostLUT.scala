// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

final case class GuideCameraTimes(gMag: Double, poorWeather: Double, goodWeather: Double)
final case class SVCameraTimes(gMag: Double, poorWeather: Double, goodWeather: Double)

// GHOST Lookup tables
trait GhostLUT {
  val GuideCameraTimesLUT =
    List(
      GuideCameraTimes(03.0, 00.1, 00.1),
      GuideCameraTimes(03.5, 00.1, 00.1),
      GuideCameraTimes(04.0, 00.2, 00.1),
      GuideCameraTimes(04.5, 00.2, 00.1),
      GuideCameraTimes(05.0, 00.2, 00.1),
      GuideCameraTimes(05.5, 00.3, 00.2),
      GuideCameraTimes(06.0, 00.3, 00.2),
      GuideCameraTimes(06.5, 00.4, 00.2),
      GuideCameraTimes(07.0, 00.5, 00.2),
      GuideCameraTimes(07.5, 00.6, 00.3),
      GuideCameraTimes(08.0, 00.7, 00.3),
      GuideCameraTimes(08.5, 00.8, 00.4),
      GuideCameraTimes(09.0, 00.9, 00.5),
      GuideCameraTimes(09.5, 01.0, 00.6),
      GuideCameraTimes(10.0, 01.0, 00.7),
      GuideCameraTimes(10.5, 02.0, 00.8),
      GuideCameraTimes(11.0, 02.0, 00.9),
      GuideCameraTimes(11.5, 02.0, 01.0),
      GuideCameraTimes(12.0, 03.0, 01.0),
      GuideCameraTimes(12.5, 03.0, 02.0),
      GuideCameraTimes(13.0, 04.0, 02.0),
      GuideCameraTimes(13.5, 05.0, 02.0),
      GuideCameraTimes(14.0, 06.0, 02.0),
      GuideCameraTimes(14.5, 07.0, 03.0),
      GuideCameraTimes(15.0, 08.0, 03.0),
      GuideCameraTimes(15.5, 10.0, 04.0),
      GuideCameraTimes(16.0, 11.0, 05.0),
      GuideCameraTimes(16.5, 14.0, 06.0),
      GuideCameraTimes(17.0, 16.0, 06.0),
      GuideCameraTimes(17.5, 19.0, 08.0),
      GuideCameraTimes(18.0, 23.0, 09.0),
      GuideCameraTimes(18.5, 28.0, 11.0),
      GuideCameraTimes(19.0, 30.0, 12.0),
      GuideCameraTimes(19.5, 40.0, 15.0),
      GuideCameraTimes(20.0, 50.0, 17.0)
    ).sortBy(_.gMag)

  // the List is never empty
  val AGMinimumTime: Double = GuideCameraTimesLUT.find(_.gMag == 17.0).map(_.goodWeather).get

  val SVCameraTimesLUT =
    List(
      SVCameraTimes(3.0, 0.2, 0.2),
      SVCameraTimes(3.5, 0.2, 0.2),
      SVCameraTimes(4.0, 0.3, 0.2),
      SVCameraTimes(4.5, 0.3, 0.3),
      SVCameraTimes(5.0, 0.4, 0.3),
      SVCameraTimes(5.5, 0.6, 0.4),
      SVCameraTimes(6.0, 0.7, 0.6),
      SVCameraTimes(6.5, 1.0, 0.7),
      SVCameraTimes(7.0, 1.0, 1.0),
      SVCameraTimes(7.5, 2.0, 1.0),
      SVCameraTimes(8.0, 2.0, 2.0),
      SVCameraTimes(8.5, 3.0, 2.0),
      SVCameraTimes(9.0, 4.0, 3.0),
      SVCameraTimes(9.5, 5.0, 3.0),
      SVCameraTimes(10.0, 6.0, 4.0),
      SVCameraTimes(10.5, 8.0, 6.0),
      SVCameraTimes(11.0, 10.0, 7.0),
      SVCameraTimes(11.5, 14.0, 9.0),
      SVCameraTimes(12.0, 18.0, 12.0),
      SVCameraTimes(12.5, 23.0, 15.0),
      SVCameraTimes(13.0, 30.0, 20.0),
      SVCameraTimes(13.5, 40.0, 25.0),
      SVCameraTimes(14.0, 50.0, 30.0),
      SVCameraTimes(14.5, 70.0, 40.0),
      SVCameraTimes(15.0, 90.0, 50.0),
      SVCameraTimes(15.5, 110.0, 70.0),
      SVCameraTimes(16.0, 150.0, 90.0),
      SVCameraTimes(16.5, 190.0, 110.0),
      SVCameraTimes(17.0, 250.0, 150.0),
      SVCameraTimes(17.5, 300.0, 190.0),
      SVCameraTimes(18.0, 300.0, 240.0),
      SVCameraTimes(18.5, 300.0, 300.0),
      SVCameraTimes(19.0, 300.0, 300.0),
      SVCameraTimes(19.5, 300.0, 300.0),
      SVCameraTimes(20.0, 300.0, 300.0)
    ).sortBy(_.gMag)

  // the List is never empty
  val SVMinimumTime = SVCameraTimesLUT.minBy(_.goodWeather)

}

object GhostLUT extends GhostLUT
