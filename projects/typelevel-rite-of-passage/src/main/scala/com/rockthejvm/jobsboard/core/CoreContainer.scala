package com.rockthejvm.jobsboard.core

import cats.effect.kernel.{Resource, Sync}
import org.typelevel.log4cats.Logger

import com.rockthejvm.jobsboard.core.application.ports.out.{
  JobRepository,
  TimeAdapter
}
import com.rockthejvm.jobsboard.core.application.services.JobService
import com.rockthejvm.jobsboard.core.application.services.impl.LiveJobService

trait AdapterContainer[F[_]: Sync: Logger] {
  val jobRepo: JobRepository[F]
  val timeAdapter: TimeAdapter[F]
}

final class ServiceContainer[F[_]: Sync] private (
    val jobs: JobService[F]
)

object ServiceContainer {
  def apply[F[_]: Sync: Logger](
      adapters: AdapterContainer[F]
  ): Resource[F, ServiceContainer[F]] =
    for {
      jobs <- LiveJobService(
        jobRepo = adapters.jobRepo,
        timeAdapter = adapters.timeAdapter
      )
    } yield new ServiceContainer[F](jobs)
}

final class CoreContainer[F[_]: Sync] private (
    val services: ServiceContainer[F]
)

object CoreContainer {
  // This is where command/event handlers (application services) and perhaps
  // domain services can be instantiated and stored in a CoreContainer class

  def apply[F[_]: Sync: Logger](
      adapters: AdapterContainer[F]
  ): Resource[F, CoreContainer[F]] =
    for {
      services <- ServiceContainer(adapters)
    } yield new CoreContainer(services)
}
