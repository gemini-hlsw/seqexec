// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json

package object instances {

  object all
    extends AsterismJson
       with CoAddsJson
       with CoordinatesJson
       with DynamicConfigJson
       with EnumeratedJson
       with EphemerisJson
       with GcalConfigJson
       with IndexJson
       with NonEmptySetJson
       with ObservationJson
       with OffsetJson
       with ProgramIdJson
       with ProgramJson
       with ProperMotionJson
       with SortedMapJson
       with SortedSetJson
       with StaticConfigJson
       with StepJson
       with TargetEnvironmentJson
       with TargetJson
       with TimeJson
       with WavelengthJson

}

