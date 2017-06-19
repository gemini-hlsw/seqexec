/*
 * Copyright (c) 2017, Association of Universities for Research in Astronomy, Inc. (AURA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package gem
package dao

import doobie.imports._

import gem.enum.Instrument
import gem.config._

import scalaz._
import Scalaz._


object StaticConfigDao {

  def insert(s: StaticConfig): ConnectionIO[Int] =
    for {
      id <- Statements.insertBaseSlice(s.instrument).withUniqueGeneratedKeys[Int]("static_id")
      _  <- insertConfigSlice(id, s)
    } yield id

  private def insertConfigSlice(id: Int, s: StaticConfig): ConnectionIO[Int] =
    s match {
      case _: AcqCamStaticConfig   => 0.point[ConnectionIO]
      case _: BhrosStaticConfig    => 0.point[ConnectionIO]
      case f2: F2StaticConfig      => Statements.insertF2(id, f2).run
      case _: GmosNStaticConfig    => 0.point[ConnectionIO]
      case _: GmosSStaticConfig    => 0.point[ConnectionIO]
      case _: GnirsStaticConfig    => 0.point[ConnectionIO]
      case _: GpiStaticConfig      => 0.point[ConnectionIO]
      case _: GsaoiStaticConfig    => 0.point[ConnectionIO]
      case _: MichelleStaticConfig => 0.point[ConnectionIO]
      case _: NiciStaticConfig     => 0.point[ConnectionIO]
      case _: NifsStaticConfig     => 0.point[ConnectionIO]
      case _: NiriStaticConfig     => 0.point[ConnectionIO]
      case _: PhoenixStaticConfig  => 0.point[ConnectionIO]
      case _: TrecsStaticConfig    => 0.point[ConnectionIO]
      case _: VisitorStaticConfig  => 0.point[ConnectionIO]
    }

  def select(i: Instrument, sid: Int): ConnectionIO[StaticConfig] = {
    def point(sc: StaticConfig): ConnectionIO[StaticConfig] =
      sc.point[ConnectionIO]

    i match {
      case Instrument.AcqCam     => point(AcqCamStaticConfig())
      case Instrument.Bhros      => point(BhrosStaticConfig())

      case Instrument.Flamingos2 => Statements.selectF2(sid).unique.widen[StaticConfig]

      case Instrument.GmosN      => point(GmosNStaticConfig())
      case Instrument.GmosS      => point(GmosSStaticConfig())
      case Instrument.Gnirs      => point(GnirsStaticConfig())
      case Instrument.Gpi        => point(GpiStaticConfig())
      case Instrument.Gsaoi      => point(GsaoiStaticConfig())
      case Instrument.Michelle   => point(MichelleStaticConfig())
      case Instrument.Nici       => point(NiciStaticConfig())
      case Instrument.Nifs       => point(NifsStaticConfig())
      case Instrument.Niri       => point(NiriStaticConfig())
      case Instrument.Phoenix    => point(PhoenixStaticConfig())
      case Instrument.Trecs      => point(TrecsStaticConfig())
      case Instrument.Visitor    => point(VisitorStaticConfig())
    }
  }


  object Statements {
    def selectF2(sid: Int): Query0[F2StaticConfig] =
      sql"""
        SELECT mos_preimaging
          FROM static_f2
         WHERE static_id = $sid AND instrument = ${Instrument.Flamingos2: Instrument}
      """.query[F2StaticConfig]

    def insertBaseSlice(i: Instrument): Update0 =
      sql"""
        INSERT INTO static_config (instrument)
        VALUES ($i)
      """.update

    def insertF2(id: Int, f2: F2StaticConfig): Update0 =
      sql"""
        INSERT INTO static_f2 (static_id, instrument, mos_preimaging)
        VALUES (
          $id,
          ${Instrument.Flamingos2: Instrument},
          ${f2.mosPreImaging})
      """.update
  }

}
