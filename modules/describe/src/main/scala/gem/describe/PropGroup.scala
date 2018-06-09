// package gem
// package describe

// import gem.config._
// import gem.Step.{Gcal, Science, Smart}

// import cats._, cats.data._, cats.implicits._

// /** Groups a collection of properties for some Describe-able type A.  Provides
//   * access to `StepLens`es for getting and setting property values across a
//   * sequence or list of steps. */
// case class PropGroup[A: Describe, I: Describe](aLens: Step[I] @?> A) {
//   val props: List[Prop[A]] =
//     implicitly[Describe[A]].props.sortBy(_.meta.attrs.label)

//   val stepLenses: List[StepLens[A, I]] =
//     props.map(StepLens(_, aLens))

//   val label: Option[Metadata.Label] =
//     props.headOption.flatMap(_.meta.attrs.label.parent)
// }

// object PropGroup {
//   // def groups[I: Describe](seq: Sequence[I]): List[PropGroup[_, I]] =
//   //   groups(seq.toSteps.toList)

//   def groups[I: Describe](ss: List[Step[I]]): List[PropGroup[_, I]] = {
//     val types = (Set.empty[Step.Type]/:ss) { _ + _.stepType }

//     val groups = (List.empty[PropGroup[_, I]]/:types) { (groups, t) =>
//       t match {
//         // case Gcal    => new PropGroup[GcalUnit, I](Step.gcal)       :: groups
//         case Science => new PropGroup[TelescopeConfig, I](Step.telescope) :: groups
//         case Smart   => new PropGroup[SmartCalConfig, I](Step.smartCal)   :: groups
//         case _       => groups
//       }
//     }

//     if (types.isEmpty) Nil
//     else new PropGroup[I, I](Step.instrument) :: groups
//   }
// }