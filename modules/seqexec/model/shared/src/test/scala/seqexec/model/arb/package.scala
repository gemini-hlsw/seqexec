// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

package object arb {

  object all extends ArbClientId
    with ArbRunningStep
    with ArbNotification
    with ArbM2GuideConfig
    with ArbM1GuideConfig
    with ArbTelescopeGuideConfig
    with ArbStep
    with ArbStandardStep
    with ArbNodAndShuffleStep
    with ArbStepState
    with ArbStepConfig
    with ArbDhsTypes
    with ArbTime
    with ArbNSSubexposure
    with ArbGmosParameters
    with ArbNSRunningState
    with ArbObservationProgress
}
