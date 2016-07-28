package gem

case class StepId(oid: Observation.Id, index: Int)

trait Step {

  def id: StepId
  def observeClass: Option[ObsClass]
  def instrument: Option[Instrument]

  def isComplete: Boolean          // TODO
  def target: Option[String]       // TODO
  def observeType: Option[String]  // TODO
  def band: Option[Int]            // TODO

  def dataLabel: String //= f"$oid-$index%3d"

}

