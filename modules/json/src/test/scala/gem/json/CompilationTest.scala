// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// package gem
// package json

// import io.circe._
// import gem.config._
// import gem.enum._
// import gem.util.Enumerated

// // N.B. lots of false warnings from codec derivation.
// @SuppressWarnings(Array(
//   "org.wartremover.warts.NonUnitStatements",
//   "org.wartremover.warts.Null",
//   "org.wartremover.warts.PublicInference",
//   "org.wartremover.warts.Option2Iterable"
// )) trait CompilatonTests {

//   // Use this to assert a codec for a given type
//   def assertCodec[A](
//     implicit en: Encoder[A],
//              de: Decoder[A]
//   ): (Encoder[A], Decoder[A]) = (en, de)

//   // Ensure that generic derivations will work for parameterized types
//   def enumerated[E: Enumerated] = assertCodec[E]

//   // Sanity checks
//   assertCodec[User[ProgramRole]]
//   // assertCodec[StaticConfig]
//   // assertCodec[DynamicConfig]
//   // assertCodec[Target]
//   // assertCodec[Asterism]
//   // assertCodec[TargetEnvironment]
//   // assertCodec[Step]

// }
