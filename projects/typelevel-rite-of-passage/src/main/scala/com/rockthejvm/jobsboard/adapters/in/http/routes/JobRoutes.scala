package com.rockthejvm.jobsboard.adapters.in.http.routes

import cats.MonadThrow
import cats.effect.Concurrent
import cats.implicits.*
import io.circe.generic.auto.*
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.typelevel.log4cats.Logger

import com.rockthejvm.jobsboard.adapters.in.http.responses.FailureResponse
import com.rockthejvm.jobsboard.adapters.in.http.validation.syntax.*
import com.rockthejvm.jobsboard.adapters.in.logging.syntax.*
import com.rockthejvm.jobsboard.core.application.ports.in.{
  Command,
  CoreApplication,
  ViewModel
}

class JobRoutes[F[_]: Concurrent: Logger] private (val app: CoreApplication[F])
    extends HttpValidationDsl[F] {

  // Commands

  // POST /api/jobs/createJob { cmd }
  private val createJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "createJob" =>
      req.validate[Command.CreateJob] { cmd =>
        for {
          result <- app.createJob(cmd)
          resp   <- result match
            case Right(jobId) => Created(jobId)
            case Left(error)  => BadRequest(error)
        } yield resp
      }
  }

  // POST /api/jobs/updateJobInfo { cmd }
  private val updateJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ PUT -> Root / "updateJobInfo" =>
      req.validate[Command.UpdateJobInfo] { cmd =>
        for {
          result <- app.updateJobInfo(cmd)
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
      req.validate[Command.DeleteJob] { cmd =>
        for {
          result <- app.deleteJob(cmd)
          resp   <- result match {
            case Right(_) => Ok()
            case Left(e)  => NotFound(FailureResponse(e))
          }
        } yield resp
      }
  }

  // Queries

  // GET /api/jobs?offset=x&limit=y&filters=z
  private val allJobsRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root =>
      for {
        jobsList <- app.allJobs()
        resp     <- Ok(jobsList)
      } yield resp
  }

  // GET /api/jobs/uuid
  private val findJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / UUIDVar(id) =>
      app.findJob(id).flatMap {
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
  def apply[F[_]: Concurrent: Logger](app: CoreApplication[F]) =
    new JobRoutes[F](app)
}
