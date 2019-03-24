// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.composite

import doobie._
import gem.EphemerisKey
import gem.dao.meta._

trait EphemerisKeyComposite {
  import EnumeratedMeta._
  import FormatComposite._

  /** Map an EphemerisKey as an (EphemerisKeyType, String) pair. */
  implicit val ReadEphemerisKey: Read[EphemerisKey] =
    EphemerisKey.fromTypeAndDes.toRead

  /** Map an Option[EphemerisKey] as a nullable (EphemerisKeyType, String) pair. */
  implicit lazy val ReadOptionEphemerisKey: Read[Option[EphemerisKey]] =
    EphemerisKey.fromTypeAndDes.toOptionRead

  /** Map an EphemerisKey as an (EphemerisKeyType, String) pair. */
  implicit val WriteEphemerisKey: Write[EphemerisKey] =
    EphemerisKey.fromTypeAndDes.toWrite

  /** Map an Option[EphemerisKey] as a nullable (EphemerisKeyType, String) pair. */
  implicit lazy val WriteOptionEphemerisKey: Write[Option[EphemerisKey]] =
    EphemerisKey.fromTypeAndDes.toOptionWrite

}
object EphemerisKeyComposite extends EphemerisKeyComposite
