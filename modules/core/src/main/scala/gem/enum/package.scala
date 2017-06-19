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

// The members of this package are generated from database tables, which are the source of truth.
// See project/gen2.scala for details. Associations with other model types, as needed, are provided
// here as implicit classes wrapping the generated companion objects.
package object enum {

  /** Add mapping from Step to StepType. */
  implicit class StepTypeCompanionOps(companion: StepType.type) {
    def forStep(s: Step[_]): StepType =
      s match {
        case BiasStep(_)         => StepType.Bias
        case DarkStep(_)         => StepType.Dark
        case GcalStep(_, _)      => StepType.Gcal
        case ScienceStep(_, _)   => StepType.Science
        case SmartGcalStep(_, _) => StepType.SmartGcal
      }
  }

  /** Add fold on SmartGcalType. */
  implicit class SmartGcalTypeOps(t: SmartGcalType) {
    def fold[X](lamp: GcalLampType => X, baseline: GcalBaselineType => X): X =
      t match {
        case SmartGcalType.Arc           => lamp(GcalLampType.Arc)
        case SmartGcalType.Flat          => lamp(GcalLampType.Flat)
        case SmartGcalType.DayBaseline   => baseline(GcalBaselineType.Day)
        case SmartGcalType.NightBaseline => baseline(GcalBaselineType.Night)
      }
  }
}
