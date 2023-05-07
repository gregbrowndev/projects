package com.rockthejvm.jobsboard.adapters.in.http

import cats._
import cats.effect.{IO, IOApp}
import cats.implicits._
import org.http4s.ember.server.EmberServerBuilder
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderException

import com.rockthejvm.jobsboard.AppContainer
import com.rockthejvm.jobsboard.adapters.in.config.syntax.*
import com.rockthejvm.jobsboard.adapters.in.config.{
  AppConfig,
  EmberConfig,
  PostgresConfig
}
import com.rockthejvm.jobsboard.adapters.in.http.HttpApi

object Application extends IOApp.Simple {
  /*
  Run application with `sbt run`. Note, this works because the build.sbt file
  contains Compile / runMain := "path/to/entrypoint"
   */

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override def run: IO[Unit] =
    val serverResource = for {
      appContainer <- AppContainer[IO]
      httpApi      <- HttpApi[IO](appContainer)
      server       <- EmberServerBuilder
        .default[IO]
        .withHost(appContainer.config.emberConfig.host)
        .withPort(appContainer.config.emberConfig.port)
        .withHttpApp(httpApi.routes.orNotFound)
        .build
    } yield server

    serverResource.use(_ => IO.println("Server ready!") *> IO.never)
}
