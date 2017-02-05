package gem.ocs2

package object pio {
  type PioParse[A] = String => Option[A]
}
