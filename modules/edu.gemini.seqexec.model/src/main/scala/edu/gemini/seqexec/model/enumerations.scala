// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

/**
 * Unfortunatly we'll have to store this here too to avoid exposing
 * spModel to the client
 * Should be gone when we integrate into gem
 */
object enumerations {

  object fpu {
    val CustomMaskKey = "CUSTOM_MASK"

    val GmosSFPU: Map[String, String] = Map(
      "FPU_NONE" -> "None",
      "LONGSLIT_1" -> "Longslit 0.25 arcsec",
      "LONGSLIT_2" -> "Longslit 0.50 arcsec",
      "LONGSLIT_3" -> "Longslit 0.75 arcsec",
      "LONGSLIT_4" -> "Longslit 1.00 arcsec",
      "LONGSLIT_5" -> "Longslit 1.50 arcsec",
      "LONGSLIT_6" -> "Longslit 2.00 arcsec",
      "LONGSLIT_7" -> "Longslit 5.00 arcsec",
      "IFU_1" -> "IFU 2 Slits",
      "IFU_2" -> "IFU Left Slit (blue)",
      "IFU_3" -> "IFU Right Slit (red)",
      "BHROS" -> "bHROS",

      "IFU_N" -> "IFU N and S 2 Slits",
      "IFU_N_B" -> "IFU N and S Left Slit (blue)",
      "IFU_N_R" -> "IFU N and S Right Slit (red)",

      "NS_1" -> "N and S 0.50 arcsec",
      "NS_2" -> "N and S 0.75 arcsec",
      "NS_3" -> "N and S 1.00 arcsec",
      "NS_4" -> "N and S 1.50 arcsec",
      "NS_5" -> "N and S 2.00 arcsec",
      "CUSTOM_MASK" -> "Custom Mask"
    )

    val GmosNFPU: Map[String, String] = Map(
      "FPU_NONE" -> "None",
      "LONGSLIT_1" -> "Longslit 0.25 arcsec",
      "LONGSLIT_2" -> "Longslit 0.50 arcsec",
      "LONGSLIT_3" -> "Longslit 0.75 arcsec",
      "LONGSLIT_4" -> "Longslit 1.00 arcsec",
      "LONGSLIT_5" -> "Longslit 1.50 arcsec",
      "LONGSLIT_6" -> "Longslit 2.00 arcsec",
      "LONGSLIT_7" -> "Longslit 5.00 arcsec",
      "IFU_1" -> "IFU 2 Slits",
      "IFU_2" -> "IFU Left Slit (blue)",
      "IFU_3" -> "IFU Right Slit (red)",
      "NS_0" -> "N and S 0.25 arcsec",
      "NS_1" -> "N and S 0.50 arcsec",
      "NS_2" -> "N and S 0.75 arcsec",
      "NS_3" -> "N and S 1.00 arcsec",
      "NS_4" -> "N and S 1.50 arcsec",
      "NS_5" -> "N and S 2.00 arcsec",
      "CUSTOM_MASK" -> "Custom Mask"
    )

    val Flamingos2: Map[String, String] = Map(
      "FPU_NONE" -> "Imaging (none)",
      "LONGSLIT_1" -> "1-pix longslit",
      "LONGSLIT_2" -> "2-pix longslit",
      "LONGSLIT_3" -> "3-pix longslit",
      "LONGSLIT_4" -> "4-pix longslit",
      "LONGSLIT_6" -> "6-pix longslit",
      "LONGSLIT_8" -> "8-pix longslit",
      "PINHOLE" -> "2-pix pinhole grid",
      "SUBPIX_PINHOLE" -> "subpix pinhole grid",
      "CUSTOM_MASK" -> "Custom Mask"
    )
  }
}
