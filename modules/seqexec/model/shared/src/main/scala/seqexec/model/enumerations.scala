// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

/**
 * Unfortunatly we'll have to store this here too to avoid exposing
 * spModel to the client
 * Should be gone when we integrate into gem
 */
object enumerations {
  object filter {
    val GmosSFilter: Map[String, String] = Map(
      "NONE" -> "None",
      "u_G0332" -> "u",
      "g_G0325" -> "g",
      "r_G0326" -> "r",
      "i_G0327" -> "i",
      "z_G0328" -> "z",
      "Z_G0343" -> "Z",
      "Y_G0344" -> "Y",
      "GG455_G0329" -> "GG455",
      "OG515_G0330" -> "OG515",
      "RG610_G0331" -> "RG610",
      "RG780_G0334" -> "RG780",
      "CaT_G0333" -> "CaT",
      "HartmannA_G0337_r_G0326" -> "HartmannA + r",
      "HartmannB_G0338_r_G0326" -> "HartmannB + r",
      "g_G0325_GG455_G0329" -> "g + GG455",
      "g_G0325_OG515_G0330" -> "g + OG515",
      "r_G0326_RG610_G0331" -> "r + RG610",
      "i_G0327_RG780_G0334" -> "i + RG780",
      "i_G0327_CaT_G0333" -> "i + CaT",
      "z_G0328_CaT_G0333" -> "z + CaT",
      "Ha_G0336" -> "Ha",
      "SII_G0335" -> "SII",
      "HaC_G0337" -> "HaC",
      "OIII_G0338" -> "OIII",
      "OIIIC_G0339" -> "OIIIC",
      "HeII_G0340" -> "HeII",
      "HeIIC_G0341" -> "HeIIC",
      "Lya395_G0342" -> "Lya395",
      "OVI_G0347" -> "OVI",
      "OVIC_G0348" -> "OVIC"
    )

    val GmosNFilter: Map[String, String] = Map(
      "NONE" -> "None",
      "g_G0301" -> "g",
      "r_G0303" -> "r",
      "i_G0302" -> "i",
      "z_G0304" -> "z",
      "Z_G0322" -> "Z",
      "Y_G0323" -> "Y",
      "GG455_G0305" -> "GG455",
      "OG515_G0306" -> "OG515",
      "RG610_G0307" -> "RG610",
      "CaT_G0309" -> "CaT",
      "Ha_G0310" -> "Ha",
      "HaC_G0311" -> "HaC",
      "DS920_G0312" -> "DS920",
      "SII_G0317" -> "SII",
      "OIII_G0318" -> "OIII",
      "OIIIC_G0319" -> "OIIIC",
      "HeII_G0320" -> "HeII",
      "HeIIC_G0321" -> "HeIIC",
      "OVI_G0345" -> "OVI",
      "OVIC_G0346" -> "OVIC",
      "HartmannA_G0313_r_G0303" -> "HartmannA_G0313 + r",
      "HartmannB_G0314_r_G0303" -> "HartmannB_G0314 + r",
      "g_G0301_GG455_G0305" -> "g + GG455",
      "g_G0301_OG515_G0306" -> "g + OG515",
      "r_G0303_RG610_G0307" -> "r + RG610",
      "i_G0302_CaT_G0309" -> "i + CaT",
      "z_G0304_CaT_G0309" -> "z + CaT",
      "u_G0308" -> "u"
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

    val Niri: Map[String, String] = Map(
      "BBF_Y" -> "Y (1.02 um)",
      "BBF_J" -> "J (1.25 um)",
      "BBF_H" -> "H (1.65 um)",
      "BBF_KPRIME" -> "K(prime) (2.12 um)",
      "BBF_KSHORT" -> "K(short) (2.15 um)",
      "BBF_K" -> "K (2.20 um)",
      "BBF_LPRIME" -> "L(prime) (3.78 um)",
      "BBF_MPRIME" -> "M(prime) (4.68 um)",
      "BBF_J_ORDER_SORT" -> "Order sorting J (1.30 um)",
      "BBF_H_ORDER_SORT" -> "Order sorting H (1.69 um)",
      "BBF_K_ORDER_SORT" -> "Order sorting K (2.20 um)",
      "BBF_L_ORDER_SORT" -> "Order sorting L (3.50 um)",
      "BBF_M_ORDER_SORT" -> "Order sorting M (5.00 um)",
      "J_CONTINUUM_106" -> "J-continuum (1.065 um)",
      "NBF_HEI" -> "HeI (1.083 um)",
      "NBF_PAGAMMA" -> "Pa(gamma) (1.094 um)",
      "J_CONTINUUM_122" -> "J-continuum (1.122 um)",
      "NBF_H" -> "J-continuum (1.207 um)",
      "NBF_PABETA" -> "Pa(beta) (1.282 um)",
      "NBF_HCONT" -> "H-continuum (1.570 um)",
      "NBF_CH4SHORT" -> "CH4(short) (1.56 um)",
      "NBF_CH4LONG" -> "CH4(long) (1.70 um)",
      "NBF_FEII" -> "[FeII] (1.644 um)",
      "NBF_H2O_2045" -> "H2O ice (2.045 um)",
      "NBF_HE12P2S" -> "HeI (2p2s) (2.059 um)",
      "NBF_KCONT1" -> "K-continuum (2.09 um)",
      "NBF_H210" -> "H2 1-0 S(1) (2.122 um)",
      "NBF_BRGAMMA" -> "Br(gamma) (2.166 um)",
      "NBF_H221" -> "H2 2-1 S(1) (2.248 um)",
      "NBF_KCONT2" -> "K-continuum (2.27 um)",
      "NBF_CH4ICE" -> "CH4 ice (2.275 um)",
      "NBF_CO20" -> "CO 2-0 (bh) (2.294 um)",
      "NBF_CO31" -> "CO 3-1 (bh) (2.323 um)",
      "NBF_H2O" -> "H2O ice (3.050 um)",
      "NBF_HC" -> "hydrocarbon (3.295 um)",
      "NBF_BRACONT" -> "Br(alpha) cont (3.990 um)",
      "NBF_BRA" -> "Br(alpha) (4.052 um)"
    )

  }

  object camera {

    val Niri: Map[String, String] = Map(
      "F6" -> "f/6 (0.12 arcsec/pix)",
      "F14" -> "f/14 (0.05 arcsec/pix)",
      "F32" -> "f/32 (0.02 arcsec/pix)",
      "F32_PV" -> "f/32 + pupil viewer"
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

  object disperser {
    val GmosSDisperser: Map[String, String] = Map(
      "MIRROR"      -> "Mirror",
      "B1200_G5321" -> "B1200",
      "R831_G5322"  -> "R831",
      "B600_G5323"  -> "B600",
      "R600_G5324"  -> "R600",
      "R400_G5325"  -> "R400",
      "R150_G5326"  -> "R150"
    )

    val GmosNDisperser: Map[String, String] = Map(
      "MIRROR"      -> "Mirror",
      "B1200_G5301" -> "B1200",
      "R831_G5302"  -> "R831",
      "B600_G5307"  -> "B600",
      "R600_G5304"  -> "R600",
      "R400_G5305"  -> "R400",
      "R150_G5308"  -> "R150"
    )
  }
}
