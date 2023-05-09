package com.rockthejvm.jobsboard.core

import cats.effect.kernel.{Async, Resource}

import com.rockthejvm.jobsboard.core.application.LiveCoreApplication
import com.rockthejvm.jobsboard.core.application.ports.in.CoreApplication
import com.rockthejvm.jobsboard.core.application.ports.out.JobRepository

trait AdapterContainer[F[_]: Async] {
  val jobRepo: JobRepository[F]
}

final class CoreContainer[F[_]: Async] private (
    val app: CoreApplication[F]
)

object CoreContainer {
  // This is where command/event handlers (application services) and perhaps
  // domain services can be instantiated and stored in a CoreContainer class
  def apply[F[_]: Async](
      adapters: AdapterContainer[F]
  ): Resource[F, CoreContainer[F]] =
    for {
      app <- LiveCoreApplication(jobRepo = adapters.jobRepo)
    } yield new CoreContainer(app)
}
