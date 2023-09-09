package com.rockthejvm.jobsboard.adapters.in.http

import cats.*
import cats.data.OptionT
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.*
import org.http4s.HttpRoutes
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.{ErrorAction, ErrorHandling}
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

object Application extends IOApp {
  /* Run application with `sbt run`. Note, this works because the build.sbt file
   * contains Compile / runMain := "path/to/entrypoint" */

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override def run(args: List[String]): IO[ExitCode] =
    val serverResource = for {
      appContainer <- AppContainer[IO]
      httpApi      <- HttpApi[IO](jobService = appContainer.core.services.jobs)
      server       <- EmberServerBuilder
        .default[IO]
        .withHost(appContainer.config.emberConfig.host)
        .withPort(appContainer.config.emberConfig.port)
        .withHttpApp(withErrorLogging(httpApi.routes).orNotFound)
        .build
    } yield server

    serverResource
      .use(_ => IO.println("Server ready!") *> IO.never)
      .as(ExitCode.Success)

  private def withErrorLogging(routes: HttpRoutes[IO]): HttpRoutes[IO] =
    ErrorHandling.Recover.total(
      ErrorAction.log(
        routes,
        messageFailureLogAction =
          (t, msg) => OptionT.liftF(IO(println(msg)) >> IO(println(t))),
        serviceErrorLogAction =
          (t, msg) => OptionT.liftF(IO(println(msg)) >> IO(println(t)))
      )
    )
}
