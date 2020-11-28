// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package ocs2.config

import lucuma.core.enum._
import io.chrisdavenport.cats.time.instances.all._
import lucuma.core.math.{ Offset, Wavelength }

import cats.{ Eq, Order }
import java.time.Duration
import monocle._

/**
  * Additional type hierarchy over the low-level GMOS enums.
  * @group Instrument-Specific Models
  */
object GmosConfig {

  /** Nod-and-shuffle offset in detector rows, which must be positive, non-zero.
    * This class essentially provides a newtype for Int.
    */
  sealed abstract case class GmosShuffleOffset(detectorRows: Int) {

    // Enforced by fromRowCount constructor
    assert(detectorRows > 0, s"detectorRows must be > 0, not $detectorRows")
  }

  object GmosShuffleOffset extends GmosShuffleOffsetOptics {

    /** Constructs the shuffle offset with the given number of detector rows,
      * provided it is a positive number.
      *
      * @return `Some(GmosShuffleOffset(rows))` if `rows` is positive,
      *         `None` otherwise
      */
    def fromRowCount(rows:       Int): Option[GmosShuffleOffset] =
      if (rows > 0) Some(new GmosShuffleOffset(rows) {}) else None

    /** Constructs the shuffle offset with the given number of detector rows
      * provided `rows` is positive, or throws an exception if zero or negative.
      */
    def unsafeFromRowCount(rows: Int): GmosShuffleOffset         =
      fromRowCount(rows).getOrElse(sys.error(s"Expecting positive detector row count, not $rows"))

    /** Constructs a shuffle offset using the default number of detector rows
      * associated with the detector.
      */
    def defaultFromDetector(detector: GmosDetector): GmosShuffleOffset =
      fromRowCount(detector.shuffleOffset)
        .getOrElse(sys.error(s"Misconfigured GmosDetector $detector"))

    implicit val EqualGmosShuffleOffset: Eq[GmosShuffleOffset] =
      Eq.fromUniversalEquals
  }

  trait GmosShuffleOffsetOptics {

    /** @group Optics */
    val detectorRows: Getter[GmosShuffleOffset, Int] =
      Getter(_.detectorRows)

  }

  /** The number of nod-and-shuffle cycles, which must be at least 1. This class
    * essentially provides a newtype for Int.
    */
  sealed abstract case class GmosShuffleCycles(toInt: Int) {

    // Enforced by fromCycleCount constructor
    assert(toInt > 0, s"toInt must be > 0, not $toInt")
  }

  object GmosShuffleCycles extends GmosShuffleCyclesOptics {

    /** Default non-and-shuffle cycles, which is 1. */
    val Default: GmosShuffleCycles =
      unsafeFromCycleCount(1)

    /** Constructs the shuffle cycles from a count if `cycles` is positive.
      *
      * @return `Some(GmosShuffleCycles(cycles))` if `cycles` is positive,
      *         `None` otherwise
      */
    def fromCycleCount(cycles:       Int): Option[GmosShuffleCycles] =
      if (cycles > 0) Some(new GmosShuffleCycles(cycles) {}) else None

    /** Constructs the shuffle cycles with the given `cycles` count provided it
      * is positive, or else throws an exception if 0 or negative.
      */
    def unsafeFromCycleCount(cycles: Int): GmosShuffleCycles         =
      fromCycleCount(cycles).getOrElse(sys.error(s"Expecting positive shuffle cycles, not $cycles"))

    implicit val EqualGmosShuffleCycles: Eq[GmosShuffleCycles] =
      Eq.fromUniversalEquals
  }

  trait GmosShuffleCyclesOptics {

    /** @group Optics */
    val shuffleCycles: Getter[GmosShuffleCycles, Int] =
      Getter(_.toInt)

  }

  // TODO: there are many ways to misconfigure Nod And Shuffle.  Some of these
  // ways can be caught in a companion object constructor, and some cannot, or
  // at least cannot easily.  In the first category, we should definitly check
  // whether posA and posB are close enough together (< 2 arcsecs) to allow
  // e-offsetting.  In the second category, shuffle offset in detector rows has
  // to be a multiple of y-binning.  Worse, y-binning is part of the dynamic
  // configuration so it isn't clear what happens if the shuffle offset isn't
  // always a multiple of y-binning.

  /** GMOS nod-and-shuffle configuration. */
  final case class GmosNodAndShuffle(
    posA:    Offset,
    posB:    Offset,
    eOffset: GmosEOffsetting,
    shuffle: GmosShuffleOffset,
    cycles:  GmosShuffleCycles
  )

  object GmosNodAndShuffle extends GmosNodAndShuffleOptics {
    val Default: GmosNodAndShuffle =
      GmosNodAndShuffle(
        Offset.Zero,
        Offset.Zero,
        GmosEOffsetting.Off,
        GmosShuffleOffset.defaultFromDetector(GmosDetector.HAMAMATSU),
        GmosShuffleCycles.Default
      )

    implicit val EqualGmosNodAndShuffle: Eq[GmosNodAndShuffle] =
      Eq.fromUniversalEquals
  }

  trait GmosNodAndShuffleOptics {

    /** @group Optics */
    val posA: Lens[GmosNodAndShuffle, Offset] =
      Lens[GmosNodAndShuffle, Offset](_.posA)(a => _.copy(posA = a))

    /** @group Optics */
    val posB: Lens[GmosNodAndShuffle, Offset] =
      Lens[GmosNodAndShuffle, Offset](_.posB)(a => _.copy(posB = a))

    /** @group Optics */
    val eOffset: Lens[GmosNodAndShuffle, GmosEOffsetting] =
      Lens[GmosNodAndShuffle, GmosEOffsetting](_.eOffset)(a => _.copy(eOffset = a))

    /** @group Optics */
    val shuffle: Lens[GmosNodAndShuffle, GmosShuffleOffset] =
      Lens[GmosNodAndShuffle, GmosShuffleOffset](_.shuffle)(a => _.copy(shuffle = a))

    /** @group Optics */
    val cycles: Lens[GmosNodAndShuffle, GmosShuffleCycles] =
      Lens[GmosNodAndShuffle, GmosShuffleCycles](_.cycles)(a => _.copy(cycles = a))

  }

  /** GMOS custom ROI entry definition. */
  sealed abstract case class GmosCustomRoiEntry(
    xMin:   Short,
    yMin:   Short,
    xRange: Short,
    yRange: Short
  ) {

    // Enforced by fromDescription constructor
    assert(xMin > 0, s"xMin must be > 0, not $xMin")
    assert(yMin > 0, s"yMin must be > 0, not $yMin")
    assert(xRange > 0, s"xRange must be > 0, not $xRange")
    assert(yRange > 0, s"yRange must be > 0, not $yRange")

    /** Columns included in this ROI entry (start, end]. */
    def columns: (Int, Int) =
      (xMin.toInt, xMin + xRange)

    /** Rows included in this ROI entry (start, end]. */
    def rows: (Int, Int) =
      (yMin.toInt, yMin + yRange)

    /** Returns `true` if the pixels specified by this custom ROI entry overlap
      * with the pixels specified by `that` entry.
      */
    def overlaps(that: GmosCustomRoiEntry): Boolean =
      columnsOverlap(that) && rowsOverlap(that)

    /** Returns `true` if the columns spanned this custom ROI entry overlap with
      * the columns spanned by `that` entry.
      */
    def columnsOverlap(that: GmosCustomRoiEntry): Boolean =
      overlapCheck(that, _.columns)

    /** Returns `true` if the rows spanned this custom ROI entry overlap with
      * the rows spanned by `that` entry.
      */
    def rowsOverlap(that: GmosCustomRoiEntry): Boolean =
      overlapCheck(that, _.rows)

    private def overlapCheck(
      that: GmosCustomRoiEntry,
      f:    GmosCustomRoiEntry => (Int, Int)
    ): Boolean = {
      val List((_, end), (start, _)) = List(f(this), f(that)).sortBy(_._1)
      end > start
    }
  }

  object GmosCustomRoiEntry extends GmosCustomRoiEntryOptics {

    def fromDescription(
      xMin:   Short,
      yMin:   Short,
      xRange: Short,
      yRange: Short
    ): Option[GmosCustomRoiEntry] =
      if ((xMin > 0) && (yMin > 0) && (xRange > 0) && (yRange > 0))
        Some(new GmosCustomRoiEntry(xMin, yMin, xRange, yRange) {})
      else
        None

    def unsafeFromDescription(
      xMin:   Short,
      yMin:   Short,
      xRange: Short,
      yRange: Short
    ): GmosCustomRoiEntry =
      fromDescription(xMin, yMin, xRange, yRange)
        .getOrElse(
          sys.error(
            s"All custom ROI fields must be > 0 in GmosCustomRoi.unsafeFromDefinition($xMin, $yMin, $xRange, $yRange)"
          )
        )

    implicit val OrderGmosCustomRoiEntry: Order[GmosCustomRoiEntry] =
      Order.by(c => (c.xMin, c.yMin, c.xRange, c.yRange))

  }

  trait GmosCustomRoiEntryOptics {

    /** @group Optics */
    val xMin: Getter[GmosCustomRoiEntry, Short] =
      Getter(_.xMin)

    /** @group Optics */
    val yMin: Getter[GmosCustomRoiEntry, Short] =
      Getter(_.yMin)

    /** @group Optics */
    val xRange: Getter[GmosCustomRoiEntry, Short] =
      Getter(_.xRange)

    /** @group Optics */
    val yRange: Getter[GmosCustomRoiEntry, Short] =
      Getter(_.yRange)

  }

  /** Shared static configuration for both GMOS-N and GMOS-S.
    */
  final case class GmosCommonStaticConfig(
    detector:      GmosDetector,
    mosPreImaging: MosPreImaging,
    nodAndShuffle: Option[GmosNodAndShuffle],
    customRois:    Set[GmosCustomRoiEntry]
  )

  object GmosCommonStaticConfig extends GmosCommonStaticConfigOptics {

    val Default: GmosCommonStaticConfig =
      GmosCommonStaticConfig(
        GmosDetector.HAMAMATSU,
        MosPreImaging.IsNotMosPreImaging,
        None,
        Set.empty[GmosCustomRoiEntry]
      )

    implicit val EqGmosCommonStaticConfig: Eq[GmosCommonStaticConfig] =
      Eq.by(c => (c.detector, c.mosPreImaging, c.nodAndShuffle, c.customRois))

  }

  trait GmosCommonStaticConfigOptics {

    /** @group Optics */
    val detector: Lens[GmosCommonStaticConfig, GmosDetector] =
      Lens[GmosCommonStaticConfig, GmosDetector](_.detector)(a => _.copy(detector = a))

    /** @group Optics */
    val mosPreImaging: Lens[GmosCommonStaticConfig, MosPreImaging] =
      Lens[GmosCommonStaticConfig, MosPreImaging](_.mosPreImaging)(a => _.copy(mosPreImaging = a))

    /** @group Optics */
    val nodAndShuffle: Lens[GmosCommonStaticConfig, Option[GmosNodAndShuffle]] =
      Lens[GmosCommonStaticConfig, Option[GmosNodAndShuffle]](_.nodAndShuffle)(a =>
        _.copy(nodAndShuffle = a)
      )

    /** @group Optics */
    val customRois: Lens[GmosCommonStaticConfig, Set[GmosCustomRoiEntry]] =
      Lens[GmosCommonStaticConfig, Set[GmosCustomRoiEntry]](_.customRois)(a =>
        _.copy(customRois = a)
      )

  }

  /** Parameters that determine GMOS CCD readout.
    */
  final case class GmosCcdReadout(
    xBinning:    GmosXBinning,
    yBinning:    GmosYBinning,
    ampCount:    GmosAmpCount,
    ampGain:     GmosAmpGain,
    ampReadMode: GmosAmpReadMode
  )

  object GmosCcdReadout extends GmosCcdReadoutOptics {

    val Default: GmosCcdReadout =
      GmosCcdReadout(
        GmosXBinning.One,
        GmosYBinning.One,
        GmosAmpCount.Twelve,
        GmosAmpGain.Low,
        GmosAmpReadMode.Slow
      )

    implicit val EqualGmosCcdReadout: Eq[GmosCcdReadout] =
      Eq.by(c => (c.xBinning, c.yBinning, c.ampCount, c.ampGain, c.ampReadMode))

  }

  trait GmosCcdReadoutOptics {

    /** @group Optics */
    val xBinning: Lens[GmosCcdReadout, GmosXBinning] =
      Lens[GmosCcdReadout, GmosXBinning](_.xBinning)(a => _.copy(xBinning = a))

    /** @group Optics */
    val yBinning: Lens[GmosCcdReadout, GmosYBinning] =
      Lens[GmosCcdReadout, GmosYBinning](_.yBinning)(a => _.copy(yBinning = a))

    /** @group Optics */
    val ampCount: Lens[GmosCcdReadout, GmosAmpCount] =
      Lens[GmosCcdReadout, GmosAmpCount](_.ampCount)(a => _.copy(ampCount = a))

    /** @group Optics */
    val ampGain: Lens[GmosCcdReadout, GmosAmpGain] =
      Lens[GmosCcdReadout, GmosAmpGain](_.ampGain)(a => _.copy(ampGain = a))

    /** @group Optics */
    val ampReadMode: Lens[GmosCcdReadout, GmosAmpReadMode] =
      Lens[GmosCcdReadout, GmosAmpReadMode](_.ampReadMode)(a => _.copy(ampReadMode = a))

  }

  /** Shared dynamic configuration for both GMOS-N and GMOS-S.
    */
  final case class GmosCommonDynamicConfig(
    ccdReadout:   GmosCcdReadout,
    dtaxOffset:   GmosDtax,
    exposureTime: Duration,
    roi:          GmosRoi
  )

  object GmosCommonDynamicConfig extends GmosCommonDynamicConfigOptics {

    val Default: GmosCommonDynamicConfig =
      GmosCommonDynamicConfig(
        GmosCcdReadout.Default,
        GmosDtax.Zero,
        Duration.ofSeconds(300),
        GmosRoi.FullFrame
      )

    implicit val EqualGmosCommonDynamicConfig: Eq[GmosCommonDynamicConfig] =
      Eq.by(c => (c.ccdReadout, c.dtaxOffset, c.exposureTime, c.roi))

  }

  trait GmosCommonDynamicConfigOptics {

    /** @group Optics */
    val ccdReadout: Lens[GmosCommonDynamicConfig, GmosCcdReadout] =
      Lens[GmosCommonDynamicConfig, GmosCcdReadout](_.ccdReadout)(a => _.copy(ccdReadout = a))

    /** @group Optics */
    val dtaxOffset: Lens[GmosCommonDynamicConfig, GmosDtax] =
      Lens[GmosCommonDynamicConfig, GmosDtax](_.dtaxOffset)(a => _.copy(dtaxOffset = a))

    /** @group Optics */
    val exposureTime: Lens[GmosCommonDynamicConfig, Duration] =
      Lens[GmosCommonDynamicConfig, Duration](_.exposureTime)(a => _.copy(exposureTime = a))

    /** @group Optics */
    val roi: Lens[GmosCommonDynamicConfig, GmosRoi] =
      Lens[GmosCommonDynamicConfig, GmosRoi](_.roi)(a => _.copy(roi = a))

  }

  /** Custom mask definition, which is available as an alternative to using a
    * builtin FPU.  Either both these parameters are set or neither are set in a
    * GMOS observation
    */
  final case class GmosCustomMask(
    maskDefinitionFilename: String,
    slitWidth:              GmosCustomSlitWidth
  )

  object GmosCustomMask extends GmosCustomMaskOptics {

    implicit val EqualGmosCustomMask: Eq[GmosCustomMask] =
      Eq.by(c => (c.maskDefinitionFilename, c.slitWidth))

  }

  trait GmosCustomMaskOptics {

    /** @group Optics */
    val maskDefinitionFilename: Lens[GmosCustomMask, String] =
      Lens[GmosCustomMask, String](_.maskDefinitionFilename)(a =>
        _.copy(maskDefinitionFilename = a)
      )

    /** @group Optics */
    val slitWidth: Lens[GmosCustomMask, GmosCustomSlitWidth] =
      Lens[GmosCustomMask, GmosCustomSlitWidth](_.slitWidth)(a => _.copy(slitWidth = a))

  }

  /** GMOS grating configuration, parameterized on the disperser type.  These
    * are grouped because they only apply when using a grating.  That is, all
    * are defined or none or defined in the dynamic config.
    *
    * @tparam D disperser type, expected to be `GmosNorthDisperser` or
    *           `GmosSouthDisperser`
    */
  final case class GmosGrating[D](
    disperser:  D,
    order:      GmosDisperserOrder,
    wavelength: Wavelength
  )

  object GmosGrating extends GmosGratingOptics {

    implicit def EqualGmosGrating[D: Eq]: Eq[GmosGrating[D]] =
      Eq.by(g => (g.disperser, g.order, g.wavelength))

  }

  trait GmosGratingOptics {

    /** @group Optics */
    def disperser[D]: Lens[GmosGrating[D], D] =
      Lens[GmosGrating[D], D](_.disperser)(a => _.copy(disperser = a))

    /** @group Optics */
    def order[D]: Lens[GmosGrating[D], GmosDisperserOrder] =
      Lens[GmosGrating[D], GmosDisperserOrder](_.order)(a => _.copy(order = a))

    /** @group Optics */
    def wavelength[D]: Lens[GmosGrating[D], Wavelength] =
      Lens[GmosGrating[D], Wavelength](_.wavelength)(a => _.copy(wavelength = a))

  }
}
