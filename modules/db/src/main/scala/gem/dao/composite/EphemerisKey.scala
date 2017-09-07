// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.composite

import doobie._
import gem.EphemerisKey
import gem.enum.EphemerisKeyType
import gem.dao.meta._

trait EphemerisKeyComposite {
  import EnumeratedMeta._

  /** Map an EphemerisKey as an (EphemerisKeyType, String) pair. */
  implicit val CompositeEphemerisKey: Composite[EphemerisKey] =
    Composite[(EphemerisKeyType, String)].imap(
      (t: (EphemerisKeyType, String)) => EphemerisKey.unsafeFromTypeAndDes(t._1, t._2))(
      (k: EphemerisKey)               => (k.keyType, k.des)
    )

}
object EphemerisKeyComposite extends EphemerisKeyComposite
