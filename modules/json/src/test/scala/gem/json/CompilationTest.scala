package gem
package json

import gem.config.{ StaticConfig, DynamicConfig } 

import argonaut._, Argonaut._, ArgonautShapeless._

trait CompilatonTests {

  def enumeratedEncode[E: Enumerated]: EncodeJson[E] = implicitly
  def enumeratedDEcode[E: Enumerated]: DecodeJson[E] = implicitly

  // Sanity check
  // TODO: this, better
  implicitly[EncodeJson[Program[Observation[StaticConfig, Step[DynamicConfig]]]]]
  implicitly[DecodeJson[Program[Observation[StaticConfig, Step[DynamicConfig]]]]]

}
