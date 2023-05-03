package com.rockthejvm.jobsboard.adapters.in.logging

import cats.MonadError
import cats.implicits.*
import org.typelevel.log4cats.Logger

object syntax {
  extension [F[_], E, A](
      fa: F[A]
  )(using me: MonadError[F, E], logger: Logger[F])
    def log(sucess: A => String, error: E => String): F[A] = fa.attemptTap {
      case Left(e)  => logger.error(error(e))
      case Right(a) => logger.info(sucess(a))
    }

    def logError(error: E => String): F[A] = fa.attemptTap {
      case Left(e) => logger.error(error(e))
      case _       => ().pure[F]
    }
}
