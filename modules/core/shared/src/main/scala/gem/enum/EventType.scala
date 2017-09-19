// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for observe [[gem.Event Event]]) types.
 * @group Enumerations (Generated)
 */
sealed abstract class EventType(
  val tag: String
) extends Product with Serializable

object EventType {

  /** @group Constructors */ case object StartSequence extends EventType("StartSequence")
  /** @group Constructors */ case object EndSequence extends EventType("EndSequence")
  /** @group Constructors */ case object StartSlew extends EventType("StartSlew")
  /** @group Constructors */ case object EndSlew extends EventType("EndSlew")
  /** @group Constructors */ case object StartVisit extends EventType("StartVisit")
  /** @group Constructors */ case object EndVisit extends EventType("EndVisit")
  /** @group Constructors */ case object StartIntegration extends EventType("StartIntegration")
  /** @group Constructors */ case object EndIntegration extends EventType("EndIntegration")
  /** @group Constructors */ case object Abort extends EventType("Abort")
  /** @group Constructors */ case object Continue extends EventType("Continue")
  /** @group Constructors */ case object Pause extends EventType("Pause")
  /** @group Constructors */ case object Stop extends EventType("Stop")

  /** All members of EventType, in canonical order. */
  val all: List[EventType] =
    List(StartSequence, EndSequence, StartSlew, EndSlew, StartVisit, EndVisit, StartIntegration, EndIntegration, Abort, Continue, Pause, Stop)

  /** Select the member of EventType with the given tag, if any. */
  def fromTag(s: String): Option[EventType] =
    all.find(_.tag === s)

  /** Select the member of EventType with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): EventType =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val EventTypeEnumerated: Enumerated[EventType] =
    new Enumerated[EventType] {
      def all = EventType.all
      def tag(a: EventType) = a.tag
    }

}