package com.rockthejvm.jobsboard.adapters.in.http.validation

import cats.MonadThrow
import cats.data.Validated.*
import cats.implicits.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.io.BadRequest
import org.http4s.implicits.*
import org.http4s.{EntityDecoder, Request, Response}
import org.typelevel.log4cats.Logger

import com.rockthejvm.jobsboard.adapters.in.http.responses.FailureResponse
import com.rockthejvm.jobsboard.adapters.in.logging.syntax.*

import validators.{ValidationResult, Validator}

object syntax {

  private def validateEntity[A](entity: A)(using
      validator: Validator[A]
  ): ValidationResult[A] =
    validator.validate(entity)

  trait HttpValidationDsl[F[_]: MonadThrow: Logger] extends Http4sDsl[F] {

    extension (req: Request[F])
      def validate[A: Validator](ifValid: A => F[Response[F]])(using
          EntityDecoder[F, A]
      ): F[Response[F]] =
        req
          .as[A]
          .logError(e => s"Failed to decode request body: ${e.getMessage}")
          .map(validateEntity)
          .flatMap {
            case Valid(a)        => ifValid(a)
            case Invalid(errors) =>
              BadRequest(errors.toList.map(_.message).mkString(", "))
          }
  }
}
