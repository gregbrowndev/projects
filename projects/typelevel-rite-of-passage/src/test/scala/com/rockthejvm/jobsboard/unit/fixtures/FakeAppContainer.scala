package com.rockthejvm.jobsboard.unit.fixtures

import cats.effect.kernel.{Resource, Sync}
import org.typelevel.log4cats.Logger

import com.rockthejvm.jobsboard.core.{AdapterContainer, CoreContainer}
import com.rockthejvm.jobsboard.unit.fixtures.adapters.FakeAdapterContainer

class FakeAppContainer[F[_]: Sync: Logger] private (
    val adapters: AdapterContainer[F],
    val core: CoreContainer[F]
)

object FakeAppContainer {
  def apply[F[_]: Sync: Logger]: Resource[F, FakeAppContainer[F]] =
    for
      adapters <- FakeAdapterContainer[F]
      core     <- CoreContainer[F](adapters)
    yield new FakeAppContainer[F](adapters, core)
}
