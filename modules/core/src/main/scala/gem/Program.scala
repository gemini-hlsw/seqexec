package gem

case class Program(id: Program.Id, title: String)

object Program {
  type Id = edu.gemini.spModel.core.ProgramId
  val  Id = edu.gemini.spModel.core.ProgramId
}
