package com.rockthejvm.jobsboard

import cats._
import cats.implicits._
import cats.effect.{IOApp, IO}
import org.http4s.ember.server.EmberServerBuilder
import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderException

import com.rockthejvm.jobsboard.config.EmberConfig
import com.rockthejvm.jobsboard.config.syntax.*
import com.rockthejvm.jobsboard.http.routes.HealthRoutes

/*
 1 - add a plain health endpoint to our app
 2 - add minimal configuration
 3 - basic http server layout
 */

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
  Note: the loadF function comes from an extension method we defined in
  jobsboard.config.syntax
   */
  override def run: IO[Unit] =
    for {
      config <- ConfigSource.default.loadF[IO, EmberConfig]
      server <- EmberServerBuilder
        .default[IO]
        .withHost(config.host)
        .withPort(config.port)
        .withHttpApp(HealthRoutes[IO].routes.orNotFound)
        .build
        .use(_ => IO.println("Server ready!") *> IO.never)
    } yield IO.unit
}
