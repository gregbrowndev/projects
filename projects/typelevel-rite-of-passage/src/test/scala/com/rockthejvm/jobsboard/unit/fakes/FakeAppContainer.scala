package com.rockthejvm.jobsboard.unit.fakes

import cats.effect.kernel.{Resource, Sync}

import com.rockthejvm.jobsboard.core.{AdapterContainer, CoreContainer}
import com.rockthejvm.jobsboard.unit.fakes.adapters.FakeAdapterContainer

class FakeAppContainer[F[_]: Sync] private (
    val adapters: AdapterContainer[F],
    val core: CoreContainer[F]
)

object FakeAppContainer {
  def apply[F[_]: Sync]: Resource[F, FakeAppContainer[F]] =
    for
      adapters <- FakeAdapterContainer[F]
      core     <- CoreContainer[F](adapters)
    yield new FakeAppContainer[F](adapters, core)
}
