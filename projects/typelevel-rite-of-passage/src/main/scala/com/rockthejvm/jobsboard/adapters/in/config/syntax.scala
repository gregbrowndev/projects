package com.rockthejvm.jobsboard.adapters.in.config

import scala.reflect.ClassTag

import cats.implicits.*
import cats.{MonadError, MonadThrow}
import pureconfig.error.ConfigReaderException
import pureconfig.{ConfigReader, ConfigSource}

object syntax {

  /**  This defines an extension method loadF that can be used to load the config
    *  more cleanly.
    *
    *  We refactored:
    *
    *  ```scala
    *  val configSource = ConfigSource.default.load[EmberConfig]
    *
    *  override def run: IO[Unit] =
    *      configSource match {
    *        case Left(errors) =>
    *          IO.raiseError(ConfigReaderException[Nothing](errors))
    *        case Right(config) =>
    *          EmberServerBuilder...  // build server
    *      }
    *  ```
    *
    *  to:
    *
    *  ```scala
    *  override def run: IO[Unit] = ConfigSource.default.loadF[IO, EmberConfig]
    *    .flatMap { config =>
    *      EmberServerBuilder...  // build server
    *    }
    *  ```
    *
    *  or using a for-comp (including EmberServerBuilder code):
    *
    *  ```scala
    *  override def run: IO[Unit] =
    *      for {
    *        config <- ConfigSource.default.loadF[IO, EmberConfig]
    *        server <- EmberServerBuilder
    *          .default[IO]
    *          .withHost(config.host)
    *          .withPort(config.port)
    *          .withHttpApp(HealthRoutes[IO].routes.orNotFound)
    *          .build
    *          .use(_ => IO.println("Server ready!") *> IO.never)
    *      } yield IO.unit
    *  ```
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
