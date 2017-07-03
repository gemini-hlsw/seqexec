// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl

import cats.effect.IO

// N.B. this isn't in cats-effect yet
@SuppressWarnings(Array("org.wartremover.warts.Var"))
final class IORef[A] private (var value: A) {
  def mod(f: A => A): IO[Unit] = IO(value = f(value))
  def read: IO[A] = IO(value)
}
object IORef {
  def apply[A](a: A): IO[IORef[A]] =
    IO(new IORef(a))
}
