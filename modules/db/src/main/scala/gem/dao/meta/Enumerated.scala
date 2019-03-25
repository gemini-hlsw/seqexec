// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gem.util.Enumerated
import scala.reflect.runtime.universe.TypeTag

trait EnumeratedMeta {

  // Enumerated by tag as DISTINCT (identifier)
  implicit def enumeratedMeta[A >: Null : TypeTag](implicit ev: Enumerated[A]): Meta[A] =
    Distinct.string("identifier").timap[A](ev.unsafeFromTag(_))(ev.tag(_))

}
object EnumeratedMeta extends EnumeratedMeta
