// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gem.CoAdds
import gsp.math.syntax.prism._

trait CoAddsMeta {

  implicit val CoAddsMetaShort: Meta[CoAdds] =
    Distinct.short("coadds").timap(CoAdds.fromShort.unsafeGet(_))(_.toShort)

}

object CoAddsMeta extends CoAddsMeta
