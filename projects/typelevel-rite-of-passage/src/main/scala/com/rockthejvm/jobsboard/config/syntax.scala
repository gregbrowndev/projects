package com.rockthejvm.jobsboard.config

import pureconfig.ConfigSource
import cats.MonadError
import cats.MonadThrow
import cats.implicits.*
import pureconfig.ConfigReader
import pureconfig.error.ConfigReaderException
import scala.reflect.ClassTag

object syntax {
  /*
  This defines an extension method loadF that can be used to load the config
  more cleanly.

  We refactored:

    val configSource = ConfigSource.default.load[EmberConfig]

    override def run: IO[Unit] =
        configSource match {
          case Left(errors) =>
            IO.raiseError(ConfigReaderException[Nothing](errors))
          case Right(config) =>
            EmberServerBuilder...  // build server
        }

  to:

    override def run: IO[Unit] = ConfigSource.default.loadF[IO, EmberConfig]
      .flatMap { config =>
        EmberServerBuilder...  // build server
      }

  or using a for-comp (including EmberServerBuilder code):

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
   */
  extension (source: ConfigSource)
    def loadF[F[_], A](using
        reader: ConfigReader[A],
        F: MonadThrow[F],
        tag: ClassTag[A]
    ): F[A] =
      F.pure(source.load[A])
        .flatMap {
          case Left(errors) => F.raiseError[A](ConfigReaderException(errors))
          case Right(value) => F.pure(value)
        }
}
