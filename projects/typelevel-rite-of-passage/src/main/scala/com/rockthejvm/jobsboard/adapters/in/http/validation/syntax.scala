package com.rockthejvm.jobsboard.adapters.in.http.validation

import cats.implicits.*
import org.http4s.{Request, Response}
import org.http4s.implicits.*

import validators.Validator

object syntax {
  extension [F[_]] (req: Request[F])
    def validate[A: Validator](ifValid: A => F[Response[F]]): F[Response[F]] = ???
}
