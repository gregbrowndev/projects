package com.rockthejvm.jobsboard.unit.fakes.adapters
import cats.effect.*

import com.rockthejvm.jobsboard.core.AdapterContainer
import com.rockthejvm.jobsboard.core.application.ports.out.JobRepository

class FakeAdapterContainer[F[_]: Sync] private (
    val jobRepo: JobRepository[F]
) extends AdapterContainer[F]

object FakeAdapterContainer {
  def apply[F[_]: Sync]: Resource[F, FakeAdapterContainer[F]] =
    for jobRepo <- FakeJobRepository[F]
    yield new FakeAdapterContainer(jobRepo)
}
