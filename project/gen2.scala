import doobie.imports._

import edu.gemini.spModel.core.Angle

import java.io.File

import java.time.Duration

import scalaz._, Scalaz._
import scalaz.effect._
import scala.reflect.runtime.universe.TypeTag

import shapeless._
import shapeless.ops.hlist._
import shapeless.ops.record._
import shapeless.syntax._
import shapeless.syntax.singleton._
import shapeless.record._

object gen2 {

  val xa = DriverManagerTransactor[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:gem",
    "postgres",
    ""
  )

  implicit val DurationMeta: Meta[Duration] =
    Meta[Long].xmap(Duration.ofMillis, _.toMillis)

  implicit val AngleMeta: Meta[Angle] =
    Meta[Double].xmap(Angle.fromArcsecs, _.toArcsecs)

  object ToDeclaration extends Poly1 {
    implicit def caseString  [S <: Symbol] = at[(S, String)  ] { case (s, _) => "  val " + s.name + ": String" }
    implicit def caseInt     [S <: Symbol] = at[(S, Int)     ] { case (s, _) => "  val " + s.name + ": Int" }
    implicit def caseBoolean [S <: Symbol] = at[(S, Boolean) ] { case (s, _) => "  val " + s.name + ": Boolean" }
    implicit def caseDouble  [S <: Symbol] = at[(S, Double)  ] { case (s, _) => "  val " + s.name + ": Double" }
    implicit def caseDuration[S <: Symbol] = at[(S, Duration)] { case (s, _) => "  val " + s.name + ": java.time.Duration" }
    implicit def caseAngle   [S <: Symbol] = at[(S, Angle)   ] { case (s, _) => "  val " + s.name + ": edu.gemini.spModel.core.Angle"}

    implicit def caseOptionAngle [S <: Symbol] = at[(S, Option[Angle] ) ] { case (s, _) => "  val " + s.name + ": Option[edu.gemini.spModel.core.Angle]" }
    implicit def caseOptionDouble[S <: Symbol] = at[(S, Option[Double]) ] { case (s, _) => "  val " + s.name + ": Option[Double]" }
  }

  object ToLiteral extends Poly1 {
    implicit val caseString       = at[String  ](a => "\"" + a + "\"")
    implicit val caseInt          = at[Int     ](a => a.toString)
    implicit val caseBoolean      = at[Boolean ](a => a.toString)
    implicit val caseDouble       = at[Double  ](a => a.toString)
    implicit val caseDuration     = at[Duration](a => s"java.time.Duration.ofMillis(${a.toMillis})")
    implicit val caseAngle        = at[Angle   ](a => s"edu.gemini.spModel.core.Angle.fromArcsecs(${a.toArcsecs})")

    implicit val caseOptionAngle  = at[Option[Angle ]](a => a.fold("Option.empty[edu.gemini.spModel.core.Angle]")(a0 => s"Some(edu.gemini.spModel.core.Angle.fromArcsecs(${a0.toArcsecs}))"))
    implicit val caseOptionDouble = at[Option[Double]](a => a.toString)
  }

  def cons[H <: HList, O <: HList, L](name: String, id: String, h: H)(
    implicit ma: Mapper.Aux[ToLiteral.type, H, O],
             ta: ToTraversable.Aux[O, List, L]
  ): String =
    h.map(ToLiteral).toList.mkString(s"  case object $id extends $name(", ", ", ")")

  def decl[H <: HList, O <: HList, L](name: String, h: H)(
    implicit ma: Mapper.Aux[ToDeclaration.type, H, O],
             ta: ToTraversable.Aux[O, List, L]
  ): String =
    h.map(ToDeclaration).toList.mkString(s"sealed abstract class $name(\n", ",\n", "\n)")

  def enum[R <: HList, F <: HList, D <: HList, V <: HList, Lub1, Lub2, L <: HList](name: String)(records: List[(String, R)])(
    implicit  f: Fields.Aux[R, F],
              d: Mapper.Aux[ToDeclaration.type, F, D],
             t1: ToTraversable.Aux[D, List, Lub1],
              v: Values.Aux[R, V],
             ma: Mapper.Aux[ToLiteral.type, V, L],
             t2: ToTraversable.Aux[L, List, Lub2]
  ): (String, String) =
    (s"$name.scala", s"""
      |package gem
      |package enum
      |
      |import scalaz.syntax.equal._
      |import scalaz.std.string._
      |
      |${decl(name, records.head._2.fields)}
      |
      |object $name {
      |
      |${records.map { case (id, r) => cons(name, id, r.values) }.mkString("\n") }
      |
      |  val all: List[$name] =
      |    List(${records.map(_._1).mkString(", ")})
      |
      |  def fromTag(s: String): Option[$name] =
      |    all.find(_.tag === s)
      |
      |  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      |  def unsafeFromTag(s: String): $name =
      |    fromTag(s).getOrElse(throw new NoSuchElementException(s))
      |
      |  implicit val ${name}Enumerated: Enumerated[$name] =
      |    new Enumerated[$name] {
      |      def all = $name.all
      |      def tag(a: $name) = a.tag
      |    }
      |
      |}
      |""".stripMargin.trim
    )

  // Still slightly boilerplatey but not too bad

  val enums: List[(String, String)] =
    List(
      enum("F2Disperser") {
        type F2DisperserRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'wavelength -> Option[Double]`.T
        val io = sql"select id, id tag, short_name, long_name, wavelength from e_f2_disperser".query[(String, F2DisperserRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("F2Filter") {
        type F2FilterRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'wavelength -> Option[Double], 'obsolete -> Boolean`.T
        val io = sql"select id, id tag, short_name, long_name, wavelength, obsolete from e_f2_filter".query[(String, F2FilterRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("F2FpUnit") {
        type F2FpUnitRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'slitWidth -> Int, 'decker -> String, 'obsolete -> Boolean`.T
        val io = sql"select id, id tag, short_name, long_name, slit_width, decker, obsolete from e_f2_fpunit".query[(String, F2FpUnitRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("F2LyotWheel") {
        type F2LyotWheelRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'plateScale -> Double, 'pixelScale -> Double, 'obsolete -> Boolean`.T
        val io = sql"select id, id tag, short_name, long_name, plate_scale, pixel_scale, obsolete from e_f2_lyot_wheel".query[(String, F2LyotWheelRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("F2ReadMode") {
        type F2ReadModeRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'description -> String, 'minimumExposureTime -> Duration, 'recommendedExposureTime -> Duration, 'readoutTime -> Duration, 'readCount -> Int, 'readNoise -> Double`.T
        val io = sql"select id, id tag, short_name, long_name, description, minimum_exposure_time, recommended_exposure_time, readout_time, read_count, read_noise from e_f2_read_mode".query[(String, F2ReadModeRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("F2WindowCover") {
        type F2WindowCoverRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        val io = sql"select id, id tag, short_name, long_name from e_f2_window_cover".query[(String, F2WindowCoverRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GcalFilter") {
        type GcalFilterRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        val io = sql"select id, id tag, short_name, long_name, obsolete from e_gcal_filter".query[(String, GcalFilterRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GcalContinuum") {
        type GcalContinuumRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        val io = sql"select id, id tag, short_name, long_name, obsolete from e_gcal_continuum".query[(String, GcalContinuumRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GcalArc") {
        type GcalArcRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        val io = sql"select id, id tag, short_name, long_name, obsolete from e_gcal_arc".query[(String, GcalArcRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GcalDiffuser") {
        type GcalDiffuserRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        val io = sql"select id, id tag, short_name, long_name, obsolete from e_gcal_diffuser".query[(String, GcalDiffuserRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GcalShutter") {
        type GcalShutterRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        val io = sql"select id, id tag, short_name, long_name, obsolete from e_gcal_shutter".query[(String, GcalShutterRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("MosPreImaging") {
        type MosPreImagingRec = Record.`'tag -> String, 'description -> String, 'toBoolean -> Boolean`.T
        val io = sql"select id, id tag, description, to_boolean from e_mos_preimaging".query[(String, MosPreImagingRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("StepType") {
        type StepTypeRec = Record.`'tag -> String`.T
        val io = sql"""
          SELECT enumlabel x, enumlabel y
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'step_type'
         """.query[(String, StepTypeRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("SmartGcalType") {
        type SmartGcalTypeRec = Record.`'tag -> String`.T
        val io = sql"""
          SELECT enumlabel x, enumlabel y
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'smart_gcal_type'
        """.query[(String, SmartGcalTypeRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GcalBaselineType") {
        type GcalBaselineTypeRec = Record.`'tag -> String`.T
        val io = sql"""
          SELECT enumlabel x, enumlabel y
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'gcal_baseline_type'
        """.query[(String, GcalBaselineTypeRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GcalLampType") {
        type GcalLampTypeRec = Record.`'tag -> String`.T
        val io = sql"""
          SELECT enumlabel x, enumlabel y
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'gcal_lamp_type'
        """.query[(String, GcalLampTypeRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosAdc") {
        type GmosAdcRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        val io = sql"""SELECT id, id tag, short_name, long_name FROM e_gmos_adc""".query[(String, GmosAdcRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosAmpCount") {
        type GmosAmpCountRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        val io = sql"""SELECT id, id tag, short_name, long_name FROM e_gmos_amp_count""".query[(String, GmosAmpCountRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosAmpGain") {
        type GmosAmpGainRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        val io = sql"""SELECT id, id tag, short_name, long_name FROM e_gmos_amp_gain""".query[(String, GmosAmpGainRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosAmpReadMode") {
        type GmosAmpReadModeRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        val io = sql"""SELECT id, id tag, short_name, long_name FROM e_gmos_amp_read_mode""".query[(String, GmosAmpReadModeRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosBuiltinRoi") {
        type GmosBuiltinRoiRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'xStart -> Int, 'yStart -> Int, 'xSize -> Int, 'ySize -> Int, 'obsolete -> Boolean`.T
        val io = sql"""SELECT id, id tag, short_name, long_name, x_start, y_start, x_size, y_size, obsolete FROM e_gmos_builtin_roi""".query[(String, GmosBuiltinRoiRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosCustomSlitWidth") {
        type GmosCustomSlitWidthRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'width -> Angle`.T
        val io = sql"""SELECT id, id tag, short_name, long_name, width FROM e_gmos_custom_slit_width""".query[(String, GmosCustomSlitWidthRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosDetector") {
        type GmosDetectorRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'northPixelSize -> Angle, 'southPixelSize -> Angle, 'suffleOffset -> Int, 'xSize -> Int, 'ySize -> Int, 'maxRois -> Int`.T
        val io = sql"""SELECT id, id tag, short_name, long_name, north_pixel_size, south_pixel_size, shuffle_offset, x_size, y_size, max_rois FROM e_gmos_detector""".query[(String, GmosDetectorRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosDisperserOrder") {
        type GmosDisperserOrderRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'count -> Int`.T
        val io = sql"""SELECT id, id tag, short_name, long_name, count FROM e_gmos_disperser_order""".query[(String, GmosDisperserOrderRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosDtax") {
        type GmosDtaxRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'dtax -> Int`.T
        val io = sql"""SELECT id, id tag, short_name, long_name, dtax FROM e_gmos_dtax""".query[(String, GmosDtaxRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosNorthDisperser") {
        type GmosNorthDisperserRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'rulingDensity -> Int, 'obsolete -> Boolean`.T
        val io = sql"""SELECT id, id tag, short_name, long_name, ruling_density, obsolete FROM e_gmos_north_disperser""".query[(String, GmosNorthDisperserRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosNorthFilter") {
        type GmosNorthFilterRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'wavelength -> Angle, 'obsolete -> Boolean`.T
        val io = sql"""SELECT id, id tag, short_name, long_name, wavelength, obsolete FROM e_gmos_north_filter""".query[(String, GmosNorthFilterRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosNorthFpu") {
        type GmosNorthFpuRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'slitWidth -> Option[Angle]`.T
        val io = sql"""SELECT id, id tag, short_name, long_name, slit_width FROM e_gmos_north_fpu""".query[(String, GmosNorthFpuRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosNorthStageMode") {
        type GmosNorthStageModeRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        val io = sql"""SELECT id, id tag, short_name, long_name, obsolete FROM e_gmos_north_stage_mode""".query[(String, GmosNorthStageModeRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosSouthDisperser") {
        type GmosSouthDisperserRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'rulingDensity -> Int, 'obsolete -> Boolean`.T
        val io = sql"""SELECT id, id tag, short_name, long_name, ruling_density, obsolete FROM e_gmos_south_disperser""".query[(String, GmosSouthDisperserRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosSouthFilter") {
        type GmosSouthFilterRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'wavelength -> Angle, 'obsolete -> Boolean`.T
        val io = sql"""SELECT id, id tag, short_name, long_name, wavelength, obsolete FROM e_gmos_south_filter""".query[(String, GmosSouthFilterRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosSouthFpu") {
        type GmosSouthFpuRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'slitWidth -> Option[Angle]`.T
        val io = sql"""SELECT id, id tag, short_name, long_name, slit_width FROM e_gmos_south_fpu""".query[(String, GmosSouthFpuRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosSouthStageMode") {
        type GmosSouthStageModeRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        val io = sql"""SELECT id, id tag, short_name, long_name, obsolete FROM e_gmos_south_stage_mode""".query[(String, GmosSouthStageModeRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosXBinning") {
        type GmosXBinningRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'count -> Int`.T
        val io = sql"""SELECT id, id tag, short_name, long_name, count FROM e_gmos_x_binning""".query[(String, GmosXBinningRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GmosYBinning") {
        type GmosYBinningRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'count -> Int`.T
        val io = sql"""SELECT id, id tag, short_name, long_name, count FROM e_gmos_y_binning""".query[(String, GmosYBinningRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("EventType") {
        type EventTypeRec = Record.`'tag -> String`.T
        val io = sql"""
          SELECT enumlabel a, enumlabel b
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'evt_type'
        """.query[(String, EventTypeRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("Instrument") {
        type InstrumentRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        val io = sql"select id, id tag, short_name, long_name, obsolete from e_instrument".query[(String, InstrumentRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("ProgramType") {
        type ProgramTypeRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        val io = sql"select id, id tag, short_name, long_name, obsolete from e_program_type".query[(String, ProgramTypeRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("Site") {
        type SiteRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        val io = sql"select id, id tag, short_name, long_name from e_site".query[(String, SiteRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("ProgramRole") {
        type ProgramRole = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        val io = sql"select id, id tag, short_name, long_name from e_program_role".query[(String, ProgramRole)].list
        io.transact(xa).unsafePerformIO
      }

    )

  def apply(dir: File): IO[Seq[File]] =
    for {
      fs <- enums.traverse { case (f, c) => IO {
              val file = new File(dir, f)
              sbt.IO.write(file, c)
              file
            }}
    } yield fs

}
