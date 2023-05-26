package com.rockthejvm.jobsboard.adapters.out.db

import cats.effect.IO
import cats.effect.implicits.*
import cats.effect.kernel.{Async, Resource}
import cats.implicits.*
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts
import doobie.util.transactor.Transactor

import com.rockthejvm.jobsboard.adapters.in.config.PostgresConfig

object TransactorFactory {
  def apply[F[_]: Async](
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
}
