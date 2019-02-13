// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.util.Zipper

import cats._
import cats.implicits._


/**
 * Each observation has a collection of guide options, where each option is a
 * guide group. One of these options is automatically managed by the auto-guide
 * star service.  The rest, if any, are manually added by the user.  There can
 * be zero or one selected groups.  The selection indicates which group will be
 * used for observing.
 *
 * @param auto   automatically managed guide group
 * @param manual manually added guide groups, where `Left` indicates that none
 *               of the manual groups is the selected group while `Right`
 *               indicates that a particular manual group (the zipper focus)
 *               should be used instead of the auto group
 *
 * @tparam G guide group type
 */
final case class GuideOptions[G](
  auto:   Option[G],
  manual: Either[List[G], Zipper[G]]
) {

  def all: List[G] = {
    def ms = manual.fold(identity, _.toList)
    auto.fold(ms)(_ :: ms)
  }

  def selected: Option[G] =
    manual.fold(_ => auto, z => Some(z.focus))

}

object GuideOptions {

  def empty[G]: GuideOptions[G] =
    GuideOptions(None, Left(Nil))

  implicit val FunctorGuideOptions: Functor[GuideOptions] =
    new Functor[GuideOptions] {
      def map[A, B](ga: GuideOptions[A])(f: A => B): GuideOptions[B] =
        GuideOptions(ga.auto.map(f), ga.manual.bimap(_.map(f), _.map(f)))
    }

  implicit def EqGuideOptions[G: Eq]: Eq[GuideOptions[G]] =
    Eq.instance { (a, b) =>
      (a.auto === b.auto) && (a.manual === b.manual)
    }
}
