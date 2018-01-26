// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.composite

import doobie._
import gem.EphemerisKey
import gem.dao.meta._

trait EphemerisKeyComposite {
  import EnumeratedMeta._
  import FormatComposite._

  /** Map an EphemerisKey as an (EphemerisKeyType, String) pair. */
  implicit val CompositeEphemerisKey: Composite[EphemerisKey] =
    EphemerisKey.fromTypeAndDes.toComposite

  /** Map an Option[EphemerisKey] as a nullable (EphemerisKeyType, String) pair. */
  implicit lazy val CompositeOptionEphemerisKey: Composite[Option[EphemerisKey]] =
    EphemerisKey.fromTypeAndDes.toOptionComposite

}
object EphemerisKeyComposite extends EphemerisKeyComposite
