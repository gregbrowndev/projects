package com.rockthejvm.jobsboard.unit.fakes.adapters
import cats.effect.*

import com.rockthejvm.jobsboard.core.AdapterContainer
import com.rockthejvm.jobsboard.core.application.ports.out.{
  JobRepository,
  TimeAdapter
}

class FakeAdapterContainer[F[_]: Sync] private (
    val jobRepo: JobRepository[F],
    val timeAdapter: TimeAdapter[F]
) extends AdapterContainer[F]

object FakeAdapterContainer {
  def apply[F[_]: Sync]: Resource[F, FakeAdapterContainer[F]] =
    for
      timeAdapter <- FakeTimeAdapter[F]
      jobRepo     <- FakeJobRepository[F]
    yield new FakeAdapterContainer(jobRepo, timeAdapter)
}
