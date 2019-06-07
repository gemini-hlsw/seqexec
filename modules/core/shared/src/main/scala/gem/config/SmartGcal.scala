// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package config

import gem.enum._
import gsp.math.Wavelength

/** Marker trait for smart gcal search keys used to lookup corresponding gcal
  * configurations.
  */
sealed trait SmartGcalSearchKey

/** Marker trait for smart gcal definition keys used to register entries in
  * a smart gcal lookup table.
  */
sealed trait SmartGcalDefinitionKey

object SmartGcalKey {

  final case class Flamingos2(
    disperser: Option[F2Disperser],
    filter:    F2Filter,
    fpu:       Option[F2Fpu]
  ) extends SmartGcalSearchKey with SmartGcalDefinitionKey

  final case class GmosCommon[D, F, U](
    disperser: Option[D],
    filter:    Option[F],
    fpu:       Option[U],
    xBinning:  GmosXBinning,
    yBinning:  GmosYBinning,
    ampGain:   GmosAmpGain
  )

  type GmosNorthCommon = GmosCommon[GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu]
  type GmosSouthCommon = GmosCommon[GmosSouthDisperser, GmosSouthFilter, GmosSouthFpu]

  final case class GmosNorthSearch(
    gmos:       GmosNorthCommon,
    wavelength: Option[Wavelength]
  ) extends SmartGcalSearchKey

  final case class GmosSouthSearch(
    gmos:       GmosSouthCommon,
    wavelength: Option[Wavelength]
  ) extends SmartGcalSearchKey

  final case class GmosDefinition[D, F, U](
    gmos:            GmosCommon[D, F, U],
    wavelengthRange: (Wavelength, Wavelength)
  )

  type GmosNorthDefinition = GmosDefinition[GmosNorthDisperser, GmosNorthFilter, GmosNorthFpu]
  type GmosSouthDefinition = GmosDefinition[GmosSouthDisperser, GmosSouthFilter, GmosSouthFpu]

  final case class Gnirs(
    acquisitionMirror: GnirsAcquisitionMirror,
    pixelScale:        GnirsPixelScale,
    disperser:         GnirsDisperser,
    fpu:               Either[GnirsFpuOther, GnirsFpuSlit],
    prism:             GnirsPrism,
    wellDepth:         GnirsWellDepth
  )

  final case class GnirsSearch(
    gnirs: Gnirs,
    wavelength: Wavelength
  ) extends SmartGcalSearchKey

  final case class GnirsDefinition(
    gnirs: Gnirs,
    wavelengthRange: (Wavelength, Wavelength)
  ) extends SmartGcalDefinitionKey

}