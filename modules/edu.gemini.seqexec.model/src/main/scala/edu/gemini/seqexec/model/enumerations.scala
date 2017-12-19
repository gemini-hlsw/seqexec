// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

/**
 * Unfortunatly we'll have to store this here too to avoid exposing
 * spModel to the client
 * Should be gone when we integrate into gem
 */
object enumerations {
  object filter {
    val GmosSFilter: Map[String, String] = Map(
      "NONE" -> "None",
      "u_G0332" -> "u_G0332",
      "g_G0325" -> "g_G0325",
      "r_G0326" -> "r_G0326",
      "i_G0327" -> "i_G0327",
      "z_G0328" -> "z_G0328",
      "Z_G0343" -> "Z_G0343",
      "Y_G0344" -> "Y_G0344",
      "GG455_G0329" -> "GG455_G0329",
      "OG515_G0330" -> "OG515_G0330",
      "RG610_G0331" -> "RG610_G0331",
      "RG780_G0334" -> "RG780_G0334",
      "CaT_G0333" -> "CaT_G0333",
      "HartmannA_G0337_r_G0326" -> "HartmannA_G0337 + r_G0326",
      "HartmannB_G0338_r_G0326" -> "HartmannB_G0338 + r_G0326",
      "g_G0325_GG455_G0329" -> "g_G0325 + GG455_G0329",
      "g_G0325_OG515_G0330" -> "g_G0325 + OG515_G0330",
      "r_G0326_RG610_G0331" -> "r_G0326 + RG610_G0331",
      "i_G0327_RG780_G0334" -> "i_G0327 + RG780_G0334",
      "i_G0327_CaT_G0333" -> "i_G0327 + CaT_G0333",
      "z_G0328_CaT_G0333" -> "z_G0328 + CaT_G0333",
      "Ha_G0336" -> "Ha_G0336",
      "SII_G0335" -> "SII_G0335",
      "HaC_G0337" -> "HaC_G0337",
      "OIII_G0338" -> "OIII_G0338",
      "OIIIC_G0339" -> "OIIIC_G0339",
      "HeII_G0340" -> "HeII_G0340",
      "HeIIC_G0341" -> "HeIIC_G0341",
      "Lya395_G0342" -> "Lya395_G0342",
      "OVI_G0347" -> "OVI_G0347",
      "OVIC_G0348" -> "OVIC_G0348"
    )

    val GmosNFilter: Map[String, String] = Map(
      "NONE" -> "None",
      "g_G0301" -> "g_G0301",
      "r_G0303" -> "r_G0303",
      "i_G0302" -> "i_G0302",
      "z_G0304" -> "z_G0304",
      "Z_G0322" -> "Z_G0322",
      "Y_G0323" -> "Y_G0323",
      "GG455_G0305" -> "GG455_G0305",
      "OG515_G0306" -> "OG515_G0306",
      "RG610_G0307" -> "RG610_G0307",
      "CaT_G0309" -> "CaT_G0309",
      "Ha_G0310" -> "Ha_G0310",
      "HaC_G0311" -> "HaC_G0311",
      "DS920_G0312" -> "DS920_G0312",
      "SII_G0317" -> "SII_G0317",
      "OIII_G0318" -> "OIII_G0318",
      "OIIIC_G0319" -> "OIIIC_G0319",
      "HeII_G0320" -> "HeII_G0320",
      "HeIIC_G0321" -> "HeIIC_G0321",
      "OVI_G0345" -> "OVI_G0345",
      "OVIC_G0346" -> "OVIC_G0346",
      "HartmannA_G0313_r_G0303" -> "HartmannA_G0313 + r_G0303",
      "HartmannB_G0314_r_G0303" -> "HartmannB_G0314 + r_G0303",
      "g_G0301_GG455_G0305" -> "g_G0301 + GG455_G0305",
      "g_G0301_OG515_G0306" -> "g_G0301 + OG515_G0306",
      "r_G0303_RG610_G0307" -> "r_G0303 + RG610_G0307",
      "i_G0302_CaT_G0309" -> "i_G0302 + CaT_G0309",
      "z_G0304_CaT_G0309" -> "z_G0304 + CaT_G0309",
      "u_G0308" -> "u_G0308"
    )

    val F2Filter: Map[String, String] = Map(
      "OPEN" -> "Open",
      "Y" -> "Y (1.02 um)",
      "F1056" -> "F1056 (1.056 um)",
      "F1063" -> "F1063 (1.063 um)",
      "J_LOW" -> "J-low (1.15 um)",
      "J" -> "J (1.25 um)",
      "H" -> "H (1.65 um)",
      "K_LONG" -> "K-long (2.20 um)",
      "K_SHORT" -> "K-short (2.15 um)",
      "K_BLUE" -> "K-blue (2.06 um)",
      "K_RED" -> "K-red (2.31 um)",
      "JH" -> "JH (spectroscopic)",
      "HK" -> "HK (spectroscopic)",
      "DARK" -> "Dark"
    )

  }

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
