package com.rockthejvm.jobsboard.unit.fixtures.adapters
import cats.effect.*
import org.typelevel.log4cats.Logger

import com.rockthejvm.jobsboard.core.AdapterContainer
import com.rockthejvm.jobsboard.core.application.ports.out.{
  JobRepository,
  TimeAdapter
}

class FakeAdapterContainer[F[_]: Sync: Logger] private (
    val jobRepo: JobRepository[F],
    val timeAdapter: TimeAdapter[F]
) extends AdapterContainer[F]

object FakeAdapterContainer {
  def apply[F[_]: Sync: Logger]: Resource[F, FakeAdapterContainer[F]] =
    for
      timeAdapter <- FakeTimeAdapter[F]
      jobRepo     <- FakeJobRepository[F]
    yield new FakeAdapterContainer(jobRepo, timeAdapter)
}
