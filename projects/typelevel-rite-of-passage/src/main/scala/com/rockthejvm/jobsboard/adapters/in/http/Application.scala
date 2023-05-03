package com.rockthejvm.jobsboard.adapters.in.http

import cats._
import cats.implicits._
import cats.effect.{IOApp, IO}
import org.http4s.ember.server.EmberServerBuilder
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderException

import com.rockthejvm.jobsboard.adapters.in.config.EmberConfig
import com.rockthejvm.jobsboard.adapters.in.config.syntax.*
import com.rockthejvm.jobsboard.adapters.in.http.HttpApi

object Application extends IOApp.Simple {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override def run: IO[Unit] =
    for {
      config <- ConfigSource.default.loadF[IO, EmberConfig]
      server <- EmberServerBuilder
        .default[IO]
        .withHost(config.host)
        .withPort(config.port)
        .withHttpApp(HttpApi[IO].routes.orNotFound)
        .build
        .use(_ => IO.println("Server ready!") *> IO.never)
    } yield IO.unit
}
