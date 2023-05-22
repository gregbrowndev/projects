package com.rockthejvm.jobsboard.adapters

import cats.effect.IO
import cats.effect.implicits.*
import cats.effect.kernel.{Async, Resource}
import cats.implicits.*
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts
import doobie.util.transactor.Transactor

import com.rockthejvm.jobsboard.adapters.in.config.{AppConfig, PostgresConfig}
import com.rockthejvm.jobsboard.adapters.out.db.LiveJobRepository
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
  def makePostgresTransactor[F[_]: Async](
      config: PostgresConfig
  ): Resource[F, HikariTransactor[F]] = for {
    // TODO - the thread pool needs to be instantiated once during start up
    ce <- ExecutionContexts.fixedThreadPool(config.nThreads)
    xa <- HikariTransactor.newHikariTransactor[F](
      "org.postgresql.Driver",
      config.url,
      config.username,
      config.password,
      ce
    )
  } yield xa

  def apply[F[_]: Async](
      config: AppConfig
  ): Resource[F, LiveGatewayContainer[F]] =
    for {
      xa <- makePostgresTransactor[F](config.postgresConfig)
    } yield new LiveGatewayContainer[F](transactor = xa)
}

final class LiveAdapterContainer[F[_]: Async] private (
    val jobRepo: JobRepository[F],
    val timeAdapter: TimeAdapter[F]
) extends AdapterContainer[F]

object LiveAdapterContainer {

  def apply[F[_]: Async](
      gatewayContainer: LiveGatewayContainer[F]
  ): Resource[F, LiveAdapterContainer[F]] =
    for {
      timeAdapter <- LiveTimeAdapter[F]
      jobRepo     <- LiveJobRepository[F](gatewayContainer.transactor, timeAdapter)
    } yield new LiveAdapterContainer(jobRepo, timeAdapter)
}
