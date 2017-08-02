// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import gem.enum._
import gem.math.{Offset, Wavelength}
import java.time.Duration

import scalaz._
import Scalaz._

/**
 * Additional type hierarchy over the low-level GMOS enums.
 * @group Instrument-Specific Models
 */
object Gmos {

  /** Nod-and-shuffle offset in detector rows.  This class essentially provides
    * a newtype for Int.
    */
  sealed abstract case class GmosShuffleOffset(detectorRows: Int)

  object GmosShuffleOffset {

    /** Constructs the shuffle offset with the given number of detector rows,
      * provided it is a positive number.
      *
      * @return `Some(GmosShuffleOffset(rows))` if `rows` is positive,
      *         `None` otherwise
      */
    def fromRowCount(rows: Int): Option[GmosShuffleOffset] =
      (rows > 0) option new GmosShuffleOffset(rows) {}

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

    implicit val EqualGmosShuffleOffset: Equal[GmosShuffleOffset] =
      Equal.equalA
  }

  /** The number of nod-and-shuffle cycles. This class essentially provides a
    * newtype for Int.
    */
  sealed abstract case class GmosShuffleCycles(toInt: Int)

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
      (cycles > 0) option new GmosShuffleCycles(cycles) {}

    /** Constructs the shuffle cycles with the given `cycles` count provided it
      * is positive, or else throws an exception if 0 or negative.
      */
    def unsafeFromCycleCount(cycles: Int): GmosShuffleCycles =
      fromCycleCount(cycles).getOrElse(sys.error(s"Expecting positive shuffle cycles, not $cycles"))

    implicit val EqualGmosShuffleCycles: Equal[GmosShuffleCycles] =
      Equal.equalA
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
        GmosEOffsetting.EOffsettingOff,
        GmosShuffleOffset.defaultFromDetector(GmosDetector.HAMAMATSU),
        GmosShuffleCycles.Default
      )
  }

  /** Shared static configuration for both GMOS-N and GMOS-S.
    */
  final case class GmosCommonStaticConfig(
    detector:      GmosDetector,
    mosPreImaging: MosPreImaging,
    nodAndShuffle: Option[GmosNodAndShuffle]
  )

  object GmosCommonStaticConfig extends GmosCommonStaticConfigLenses {
    val Default: GmosCommonStaticConfig =
      GmosCommonStaticConfig(
        GmosDetector.HAMAMATSU,
        MosPreImaging.IsNotMosPreImaging,
        None
      )
  }

  trait GmosCommonStaticConfigLenses {
    val NodAndShuffle: GmosCommonStaticConfig @> Option[GmosNodAndShuffle] =
      Lens.lensu((a, b) => a.copy(nodAndShuffle = b), _.nodAndShuffle)
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
    exposureTime: Duration
  )

  object GmosCommonDynamicConfig {
    val Default: GmosCommonDynamicConfig =
      GmosCommonDynamicConfig(
        GmosCcdReadout.Default,
        GmosDtax.Zero,
        Duration.ofSeconds(300)
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
