package com.rockthejvm.jobsboard

import cats.implicits.*
import cats.effect.{IO}
import cats.effect.kernel.{Async, Resource}
import doobie.util.transactor.Transactor
import pureconfig.ConfigSource

import com.rockthejvm.jobsboard.core.{DBContainer}
import com.rockthejvm.jobsboard.adapters.in.config.AppConfig
import com.rockthejvm.jobsboard.adapters.in.config.syntax.*
import com.rockthejvm.jobsboard.core.ports.{JobRepository}
import com.rockthejvm.jobsboard.adapters.out.db.{PostgresContainer}

final class AppContainer[F[_]: Async] private (
    val config: AppConfig,
    val dbContainer: DBContainer[F]
)

object AppContainer {
  def apply[F[_]: Async]: Resource[F, AppContainer[F]] =
    for {
      config       <- Resource.eval(ConfigSource.default.loadF[F, AppConfig])
      dbContainer  <- PostgresContainer[F](config.postgresConfig)
      appContainer <- Resource.pure(new AppContainer[F](config, dbContainer))
    } yield appContainer
}
