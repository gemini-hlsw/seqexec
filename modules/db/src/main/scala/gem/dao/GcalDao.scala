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

package gem.dao

import gem.config.GcalConfig
import gem.config.GcalConfig.GcalLamp
import doobie.imports._
import gem.enum.{GcalArc, GcalContinuum, GcalDiffuser, GcalFilter, GcalShutter}
import gem.enum.GcalArc.{ArArc, CuArArc, ThArArc, XeArc}

import java.time.Duration

object GcalDao {

  def insert(gcal: GcalConfig, step: Option[Int]): ConnectionIO[Int] =
    Statements.insert(gcal, step).withUniqueGeneratedKeys[Int]("gcal_id") // janky, hm

  def select(id: Int): ConnectionIO[Option[GcalConfig]] =
    Statements.select(id).option.map(_.flatten)

  object Statements {

    // CoAdds has a DISTINCT type due to its check constraint so we need a fine-grained mapping
    // here to satisfy the query checker.
    private case class CoAdds(toShort: Short)
    private object CoAdds {
      implicit val StepIdMeta: Meta[CoAdds] =
        Distinct.short("coadds").xmap(CoAdds(_), _.toShort)
    }

    def insert(gcal: GcalConfig, step: Option[Int]): Update0 = {
      val arcs: GcalArc => Boolean = gcal.arcs.member
      sql"""INSERT INTO gcal (step_id, continuum, ar_arc, cuar_arc, thar_arc, xe_arc, filter, diffuser, shutter, exposure_time, coadds)
            VALUES ($step, ${gcal.continuum}, ${arcs(ArArc)}, ${arcs(CuArArc)}, ${arcs(ThArArc)}, ${arcs(XeArc)}, ${gcal.filter}, ${gcal.diffuser}, ${gcal.shutter}, ${gcal.exposureTime}, ${CoAdds(gcal.coadds)})
         """.update
    }

    def select(id: Int): Query0[Option[GcalConfig]] =
      sql"""
        SELECT continuum,
               ar_arc,
               cuar_arc,
               thar_arc,
               xe_arc,
               filter,
               diffuser,
               shutter,
               exposure_time,
               coadds
          FROM gcal
         WHERE gcal_id =$id
      """.query[GcalKernel].map(_.toGcalConfig)

      private case class GcalKernel(
        continuum: Option[GcalContinuum],
        ar_arc:    Boolean,
        cuar_arc:  Boolean,
        thar_arc:  Boolean,
        xe_arc:    Boolean,
        filter:    GcalFilter,
        diffuser:  GcalDiffuser,
        shutter:   GcalShutter,
        expTime:   Duration,
        coadds:    Short)
      {
        def toGcalConfig: Option[GcalConfig] =
          GcalLamp.fromConfig(continuum, ArArc -> ar_arc, CuArArc -> cuar_arc, ThArArc -> thar_arc, XeArc -> xe_arc).map { lamp =>
            GcalConfig(lamp, filter, diffuser, shutter, expTime, coadds)
          }
      }
  }

}
