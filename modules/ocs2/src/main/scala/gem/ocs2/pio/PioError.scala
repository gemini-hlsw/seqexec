package gem.ocs2.pio

sealed trait PioError extends Product with Serializable

object PioError {
  final case class MissingKey(name: String)                    extends PioError
  final case class ParseError(value: String, dataType: String) extends PioError

  def missingKey(name: String): PioError                    = MissingKey(name)
  def parseError(value: String, dataType: String): PioError = ParseError(value, dataType)
}
