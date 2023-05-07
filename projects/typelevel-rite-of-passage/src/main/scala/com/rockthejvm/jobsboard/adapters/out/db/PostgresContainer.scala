package com.rockthejvm.jobsboard.adapters.out.db

import cats.effect.IO
import cats.effect.implicits.*
import cats.effect.kernel.{Async, Resource}
import cats.implicits.*
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts

import com.rockthejvm.jobsboard.adapters.in.config.PostgresConfig
import com.rockthejvm.jobsboard.core.DBContainer
import com.rockthejvm.jobsboard.core.ports.JobRepository

final class PostgresContainer[F[_]: Async] private (
    val jobRepo: JobRepository[F]
) extends DBContainer[F]

object PostgresContainer {
  def makePostgresResource[F[_]: Async](
      config: PostgresConfig
  ): Resource[F, HikariTransactor[F]] = for {
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
      config: PostgresConfig
  ): Resource[F, PostgresContainer[F]] =
    for {
      xa      <- makePostgresResource[F](config)
      jobRepo <- LiveJobRepository[F](xa)
    } yield new PostgresContainer(jobRepo)
}
