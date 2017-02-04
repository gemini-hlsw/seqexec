package gem.seqimporter

package object pio {
  type PioParse[A] = String => Option[A]
}
