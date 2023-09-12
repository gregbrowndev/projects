package com.rockthejvm.jobsboard.adapters.in.http.routes

import cats.MonadThrow
import cats.effect.Concurrent
import cats.implicits.*
import io.circe.jawn
import org.http4s.circe.jsonOf
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.{EntityDecoder, HttpRoutes, ParseFailure, QueryParamDecoder}
import org.typelevel.log4cats.Logger

import com.rockthejvm.jobsboard.adapters.in.http.responses.FailureResponse
import com.rockthejvm.jobsboard.adapters.in.http.validation.syntax.*
import com.rockthejvm.jobsboard.adapters.in.logging.syntax.*
import com.rockthejvm.jobsboard.core.application.services.*
import com.rockthejvm.jobsboard.core.application.services.pagination.PaginationDTO

class JobRoutes[F[_]: Concurrent: Logger] private (
    val jobService: JobService[F]
) extends HttpValidationDsl[F] {

  // Automatic JSON serialisation of DTO case classes
  import io.circe.parser.decode
  import io.circe.syntax.*
  import io.circe.generic.auto.*
  import org.http4s.circe.CirceEntityCodec.*

  // Commands

  // POST /api/jobs/createJob { cmd }
  private val createJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "createJob" =>
      req.validate[CreateJobArgsDTO] { cmd =>
        for {
          result <- jobService.createJob(cmd)
          resp   <- result match
            case Right(jobId) => Created(jobId)
            case Left(error)  => BadRequest(error)
        } yield resp
      }
  }

  // POST /api/jobs/updateJobInfo { cmd }
  private val updateJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ PUT -> Root / "updateJobInfo" =>
      req.validate[UpdateJobInfoArgsDTO] { cmd =>
        for {
          result <- jobService.updateJobInfo(cmd)
          resp   <- result match {
            case Right(_) => Ok()
            case Left(e)  => NotFound(FailureResponse(e))
          }
        } yield resp
      }
  }

  // POST /api/jobs/deleteJob { cmd }
  private val deleteJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ DELETE -> Root / "deleteJob" =>
      req.validate[DeleteJobArgsDTO] { cmd =>
        for {
          result <- jobService.deleteJob(cmd)
          resp   <- result match {
            case Right(_) => Ok()
            case Left(e)  => NotFound(FailureResponse(e))
          }
        } yield resp
      }
  }

  // Queries

  object OffsetQueryParam
      extends OptionalQueryParamDecoderMatcher[Int]("offset")
  object LimitQueryParam extends OptionalQueryParamDecoderMatcher[Int]("limit")
  object FilterQueryParam
      extends OptionalQueryParamDecoderMatcher[JobFilterDTO](
        "filters"
      )

  given jobFilterQueryParamDecoder: QueryParamDecoder[JobFilterDTO] =
    QueryParamDecoder[String].emap { param =>
      decode[JobFilterDTO](param).leftMap(failure =>
        ParseFailure("Invalid JobFilter", failure.getMessage)
      )
    }

  // GET /api/jobs?offset=x&limit=y&filters=z
  private val allJobsRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ GET -> Root :? // FilterQueryParam(filter) +&
        OffsetQueryParam(
          offset
        ) +& LimitQueryParam(
          limit
        ) =>
      for {
        jobsList <- jobService.find(
          JobFilterDTO(remote = false.some),
          PaginationDTO(offset, limit)
        )
        resp     <- Ok(jobsList)
      } yield resp
  }

  // GET /api/jobs/uuid
  private val findJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / id =>
      jobService.get(id).flatMap {
        case Right(job) => Ok(job)
        case Left(err)  => NotFound(FailureResponse(err))
      }
  }

  val routes: HttpRoutes[F] = Router(
    "/jobs" -> (
      allJobsRoute
        <+> createJobRoute
        <+> findJobRoute
        <+> updateJobRoute
        <+> deleteJobRoute
    )
  )
}

object JobRoutes {
  def apply[F[_]: Concurrent: Logger](jobService: JobService[F]) =
    new JobRoutes[F](jobService)
}
