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
import com.rockthejvm.jobsboard.adapters.in.logging.syntax.*
import com.rockthejvm.jobsboard.core.application.ports.in.{
  Command,
  CoreApplication,
  ViewModel
}

class JobRoutes[F[_]: Concurrent: Logger] private (val app: CoreApplication[F])
    extends Http4sDsl[F] {

  // Commands

  // POST /api/jobs/createJob { cmd }
  private val createJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "createJob" =>
      for {
        cmd   <- req
          .as[Command.CreateJob]
          .logError(e => s"Parsing payload failed: $e")
        jobId <- app.createJob(cmd)
        resp  <- Created(jobId)
      } yield resp
  }

  // POST /api/jobs/updateJobInfo { cmd }
  private val updateJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ PUT -> Root / "updateJobInfo" =>
      for {
        cmd    <- req
          .as[Command.UpdateJobInfo]
          .logError(e => s"Parsing payload failed: $e")
        result <- app.updateJobInfo(cmd)
        resp   <- result match {
          case Right(_) => Ok()
          case Left(e)  => NotFound(FailureResponse(e))
        }
      } yield resp
  }

  // POST /api/jobs/deleteJob { cmd }
  private val deleteJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ DELETE -> Root / "deleteJob" =>
      for {
        cmd    <- req
          .as[Command.DeleteJob]
          .logError(e => s"Parsing payload failed: $e")
        result <- app.deleteJob(cmd)
        resp   <- result match {
          case Right(_) => Ok()
          case Left(e)  => NotFound(FailureResponse(e))
        }
      } yield resp
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

  val routes = Router(
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
