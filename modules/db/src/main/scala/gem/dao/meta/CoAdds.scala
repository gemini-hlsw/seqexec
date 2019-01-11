// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gem.CoAdds
import gem.syntax.prism._

trait CoAddsMeta {

  implicit val CoAddsMetaShort: Meta[CoAdds] =
    Distinct.short("coadds").timap(CoAdds.fromShort.unsafeGet(_))(_.toShort)

}

object CoAddsMeta extends CoAddsMeta
