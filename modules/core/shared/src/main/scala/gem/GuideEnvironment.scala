// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.util.Zipper

final case class GuideEnvironment[G](auto: Option[G], manual: Either[List[G], Zipper[G]]) {

  def selected: Option[G] =
    manual.fold(_ => auto, z => Some(z.focus))

}