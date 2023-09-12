package com.rockthejvm.jobsboard.adapters

import cats.effect.IO
import cats.effect.implicits.*
import cats.effect.kernel.{Async, Resource}
import cats.implicits.*
import doobie.util.transactor.Transactor
import org.typelevel.log4cats.Logger

import com.rockthejvm.jobsboard.adapters.in.config.{AppConfig, PostgresConfig}
import com.rockthejvm.jobsboard.adapters.out.db.{
  LiveJobRepository,
  TransactorFactory
}
import com.rockthejvm.jobsboard.adapters.out.time.LiveTimeAdapter
import com.rockthejvm.jobsboard.core.AdapterContainer
import com.rockthejvm.jobsboard.core.application.ports.out.{
  JobRepository,
  TimeAdapter
}

final class LiveGatewayContainer[F[_]: Async] private (
    val transactor: Transactor[F]
)

object LiveGatewayContainer {
  def apply[F[_]: Async: Logger](
      config: AppConfig
  ): Resource[F, LiveGatewayContainer[F]] =
    for {
      xa <- TransactorFactory[F](config.postgresConfig)
    } yield new LiveGatewayContainer[F](transactor = xa)
}

final class LiveAdapterContainer[F[_]: Async] private (
    val jobRepo: JobRepository[F],
    val timeAdapter: TimeAdapter[F]
) extends AdapterContainer[F]

object LiveAdapterContainer {

  def apply[F[_]: Async: Logger](
      gatewayContainer: LiveGatewayContainer[F]
  ): Resource[F, LiveAdapterContainer[F]] =
    for {
      timeAdapter <- LiveTimeAdapter[F]
      jobRepo     <- LiveJobRepository[F](gatewayContainer.transactor)
    } yield new LiveAdapterContainer(jobRepo, timeAdapter)
}
