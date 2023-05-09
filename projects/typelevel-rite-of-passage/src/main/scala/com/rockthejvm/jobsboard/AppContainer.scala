package com.rockthejvm.jobsboard

import cats.effect.IO
import cats.effect.kernel.{Async, Resource}
import cats.implicits.*
import doobie.util.transactor.Transactor
import pureconfig.ConfigSource

import com.rockthejvm.jobsboard.adapters.in.config.AppConfig
import com.rockthejvm.jobsboard.adapters.in.config.syntax.*
import com.rockthejvm.jobsboard.adapters.{
  LiveAdapterContainer,
  LiveGatewayContainer
}
import com.rockthejvm.jobsboard.core.application.ports.out.JobRepository
import com.rockthejvm.jobsboard.core.{AdapterContainer, CoreContainer}

final class AppContainer[F[_]: Async] private (
    val config: AppConfig,
    val gateways: LiveGatewayContainer[F],
    val adapters: AdapterContainer[F],
    val core: CoreContainer[F]
)

object AppContainer {
  def apply[F[_]: Async]: Resource[F, AppContainer[F]] =
    for {
      config       <- Resource.eval(ConfigSource.default.loadF[F, AppConfig])
      gateways     <- LiveGatewayContainer[F](config)
      adapters     <- LiveAdapterContainer[F](gateways)
      core         <- CoreContainer[F](adapters)
      appContainer <- Resource.pure(
        new AppContainer[F](config, gateways, adapters, core)
      )
    } yield appContainer
}
