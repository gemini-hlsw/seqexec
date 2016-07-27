package gem

case class StepId(oid: Observation.Id, index: Int)

trait Step {
  def id: StepId
  def isComplete: Boolean          // TODO
  def observeClass: Option[ObsClass] // TODO
  def target: Option[String]       // TODO
  def observeType: Option[String]  // TODO
  def band: Option[Int]            // TODO
  def instrument: Option[String]   // TODO

  def dataLabel: String //= f"$oid-$index%3d"

}

