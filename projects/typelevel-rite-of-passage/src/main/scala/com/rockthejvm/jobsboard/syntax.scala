package com.rockthejvm.jobsboard

import cats.Applicative
import cats.Applicative.*
import cats.data.EitherT
import cats.effect.implicits.*
import cats.implicits.*
import cats.syntax.all.*

object syntax {
  implicit def toEitherT[F[_], E, A](either: Either[E, A])(implicit
      F: Applicative[F]
  ): EitherT[F, E, A] =
    EitherT.fromEither[F](either)

  implicit def toFEither[F[_], E, A](eitherT: EitherT[F, E, A])(implicit
      F: Applicative[F]
  ): F[Either[E, A]] =
    eitherT.value
}
