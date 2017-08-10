// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import cats.Eq
import gem.enum._
import gem.math.{ Offset, Wavelength }
import gem.util.Lens, Lens._
import java.time.Duration

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

  object GmosShuffleOffset {

    /** Constructs the shuffle offset with the given number of detector rows,
      * provided it is a positive number.
      *
      * @return `Some(GmosShuffleOffset(rows))` if `rows` is positive,
      *         `None` otherwise
      */
    def fromRowCount(rows: Int): Option[GmosShuffleOffset] =
      if (rows > 0) Some(new GmosShuffleOffset(rows) {}) else None

    /** Constructs the shuffle offset with the given number of detector rows
      * provided `rows` is positive, or throws an exception if zero or negative.
      */
    def unsafeFromRowCount(rows: Int): GmosShuffleOffset =
      fromRowCount(rows).getOrElse(sys.error(s"Expecting positive detector row count, not $rows"))

    /** Constructs a shuffle offset using the default number of detector rows
      * associated with the detector.
      */
    def defaultFromDetector(detector: GmosDetector): GmosShuffleOffset =
      fromRowCount(detector.shuffleOffset).getOrElse(sys.error(s"Misconfigured GmosDetector $detector"))

    implicit val EqualGmosShuffleOffset: Eq[GmosShuffleOffset] =
      Eq.fromUniversalEquals
  }

  /** The number of nod-and-shuffle cycles, which must be at least 1. This class
    * essentially provides a newtype for Int.
    */
  sealed abstract case class GmosShuffleCycles(toInt: Int) {

    // Enforced by fromCycleCount constructor
    assert(toInt > 0, s"toInt must be > 0, not $toInt")
  }

  object GmosShuffleCycles {

    /** Default non-and-shuffle cycles, which is 1. */
    val Default: GmosShuffleCycles =
      unsafeFromCycleCount(1)

    /** Constructs the shuffle cycles from a count if `cycles` is positive.
      *
      * @return `Some(GmosShuffleCycles(cycles))` if `cycles` is positive,
      *         `None` otherwise
      */
    def fromCycleCount(cycles: Int): Option[GmosShuffleCycles] =
      if (cycles > 0) Some(new GmosShuffleCycles(cycles) {}) else None

    /** Constructs the shuffle cycles with the given `cycles` count provided it
      * is positive, or else throws an exception if 0 or negative.
      */
    def unsafeFromCycleCount(cycles: Int): GmosShuffleCycles =
      fromCycleCount(cycles).getOrElse(sys.error(s"Expecting positive shuffle cycles, not $cycles"))

    implicit val EqualGmosShuffleCycles: Eq[GmosShuffleCycles] =
      Eq.fromUniversalEquals
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

  object GmosNodAndShuffle {
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

  /** GMOS custom ROI entry definition. */
  sealed abstract case class GmosCustomRoiEntry(
    xMin:   Short,
    yMin:   Short,
    xRange: Short,
    yRange: Short
  ) {

    // Enforced by fromDescription constructor
    assert(xMin   > 0, s"xMin must be > 0, not $xMin")
    assert(yMin   > 0, s"yMin must be > 0, not $yMin")
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

    private def overlapCheck(that: GmosCustomRoiEntry, f: GmosCustomRoiEntry => (Int, Int)): Boolean = {
      val List((_, end), (start, _)) = List(f(this), f(that)).sortBy(_._1)
      end > start
    }
  }

  object GmosCustomRoiEntry {

    def fromDescription(xMin: Short, yMin: Short, xRange: Short, yRange: Short): Option[GmosCustomRoiEntry] =
      if ((xMin > 0) && (yMin > 0) && (xRange > 0) && (yRange > 0))
        Some(new GmosCustomRoiEntry(xMin, yMin, xRange, yRange) {})
      else
        None

    def unsafeFromDescription(xMin: Short, yMin: Short, xRange: Short, yRange: Short): GmosCustomRoiEntry =
      fromDescription(xMin, yMin, xRange, yRange)
        .getOrElse(sys.error(s"All custom ROI fields must be > 0 in GmosCustomRoi.unsafeFromDefinition($xMin, $yMin, $xRange, $yRange)"))

    implicit val EqualGmosCustomRoiEntry: Eq[GmosCustomRoiEntry] =
      Eq.fromUniversalEquals
  }

  /** Shared static configuration for both GMOS-N and GMOS-S.
    */
  final case class GmosCommonStaticConfig(
    detector:      GmosDetector,
    mosPreImaging: MosPreImaging,
    nodAndShuffle: Option[GmosNodAndShuffle],
    customRois:    Set[GmosCustomRoiEntry]
  )

  object GmosCommonStaticConfig extends GmosCommonStaticConfigLenses {
    val Default: GmosCommonStaticConfig =
      GmosCommonStaticConfig(
        GmosDetector.HAMAMATSU,
        MosPreImaging.IsNotMosPreImaging,
        None,
        Set.empty[GmosCustomRoiEntry]
      )
  }

  trait GmosCommonStaticConfigLenses {
    val CustomRois: GmosCommonStaticConfig @> Set[GmosCustomRoiEntry] =
      Lens((a, b) => a.copy(customRois = b), _.customRois)

    val NodAndShuffle: GmosCommonStaticConfig @> Option[GmosNodAndShuffle] =
      Lens((a, b) => a.copy(nodAndShuffle = b), _.nodAndShuffle)
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

  object GmosCcdReadout {
    val Default: GmosCcdReadout =
      GmosCcdReadout(
        GmosXBinning.One,
        GmosYBinning.One,
        GmosAmpCount.Twelve,
        GmosAmpGain.Low,
        GmosAmpReadMode.Slow
      )
  }

  /** Shared dynamic configuration for both GMOS-N and GMOS-S.
    */
  final case class GmosCommonDynamicConfig(
    ccdReadout:   GmosCcdReadout,
    dtaxOffset:   GmosDtax,
    exposureTime: Duration,
    roi:          GmosRoi
  )

  object GmosCommonDynamicConfig {
    val Default: GmosCommonDynamicConfig =
      GmosCommonDynamicConfig(
        GmosCcdReadout.Default,
        GmosDtax.Zero,
        Duration.ofSeconds(300),
        GmosRoi.FullFrame
      )
  }

  /** Custom mask definition, which is available as an alternative to using a
    * builtin FPU.  Either both these parameters are set or neither are set in a
    * GMOS observation
    */
  final case class GmosCustomMask(
    maskDefinitionFilename: String,
    slitWidth:              GmosCustomSlitWidth
  )

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
}
