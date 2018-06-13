// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl

import cats.effect.IO
import java.util.concurrent.atomic.AtomicReference

/** A threadsafe mutable cell. Note that there is no notion of atomicity; all oprations race. */
final class IORef[A] private (ref: AtomicReference[A]) {
  // N.B. can't compare and swap here because
  def mod(f: A => A): IO[Unit] = IO(ref.set(f(ref.get)))
  def set(a: A): IO[Unit] = IO(ref.set(a))
  def read: IO[A] = IO(ref.get)
}
object IORef {
  def apply[A](a: A): IO[IORef[A]] =
    IO(new IORef(new AtomicReference(a)))
}
