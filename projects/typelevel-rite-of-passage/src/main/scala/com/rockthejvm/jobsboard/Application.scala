package com.rockthejvm.jobsboard

import cats._
import cats.implicits._
import cats.effect.{IOApp, IO}
import org.http4s.ember.server.EmberServerBuilder
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderException

import com.rockthejvm.jobsboard.config.EmberConfig
import com.rockthejvm.jobsboard.config.syntax.*
import com.rockthejvm.jobsboard.http.HttpApi

object Application extends IOApp.Simple {
  /*
  We can compile the application and watch for changes:

    $ sbt
    sbt> ~compile

  We can then run the server in another sbt console (multiple consoles not
  exactly recommended):

    $ sbt
    sbt> runMain com.rockthejvm.jobsboard.Application

  or just:

    $ sbt "runMain com.rockthejvm.jobsboard.Application"
   */

  /*
  Note: the ConfigSource.default.loadF function comes from an extension method
  we defined in jobsboard.config.syntax
   */

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
