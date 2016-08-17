package gem

import doobie.imports._
import edu.gemini.spModel.core._
import scala.reflect.runtime.universe.TypeTag

package object dao {

  // Angle mapping to signed arcseconds. NOT implicit.
  val AngleMetaAsSignedArcseconds: Meta[Angle] =
    Meta[Double].xmap(Angle.fromArcsecs, _.toSignedDegrees * 3600)

  // OffsetP maps to a signed angle in arcseconds
  implicit val OffsetPMeta: Meta[OffsetP] =
    AngleMetaAsSignedArcseconds.xmap(OffsetP(_), _.toAngle)

  // OffsetQ maps to a signed angle in arcseconds
  implicit val OffsetQMeta: Meta[OffsetQ] =
    AngleMetaAsSignedArcseconds.xmap(OffsetQ(_), _.toAngle)

  // Program.Id as string
  implicit val ProgramIdMeta: Meta[Program.Id] =
    Meta[String].nxmap(Program.Id.parse, _.toString)

  // Observation.Id as string
  implicit val ObservationIdMeta: Meta[Observation.Id] =
    Meta[String].nxmap(Observation.Id.unsafeFromString, _.toString)

  // Enumerated by tag as string
  implicit def enumeratedMeta[A >: Null : TypeTag](implicit ev: Enumerated[A]): Meta[A] =
    Meta[String].nxmap[A](ev.unsafeFromTag(_), ev.tag(_))

}