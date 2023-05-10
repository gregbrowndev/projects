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
import com.rockthejvm.jobsboard.core.application.ports.in.CoreApplication
import com.rockthejvm.jobsboard.core.domain.job.{Job, JobId, JobInfo}

class JobRoutes[F[_]: Concurrent: Logger] private (val app: CoreApplication[F])
    extends Http4sDsl[F] {

  // POST /jobs?offset=x&limit=y { filters }
  private val allJobsRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root =>
      for {
        jobsList <- app.allJobs()
        resp     <- Ok(jobsList)
      } yield resp
  }

  // POST /jobs/create { jobInfo }
  // We can test this route with:
  /* http POST 'localhost:8080/api/jobs' <
   * ./src/main/resources/payloads/createJob.json */
  private val createJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root =>
      for {
        jobInfo <- req.as[JobInfo].logError(e => s"Parsing payload failed: $e")
        jobId   <- app.createJob(jobInfo)
        resp    <- Created(jobId)
      } yield resp
  }

  // GET /jobs/uuid
  private val findJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / UUIDVar(id) =>
      app.findJob(JobId(id)).flatMap {
        case Right(job) => Ok(job)
        case Left(err)  => NotFound(FailureResponse(err))
      }
  }

  // PUT /jobs/uuid { jobInfo }
  private val updateJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ PUT -> Root / UUIDVar(id) =>
      for {
        jobInfo <- req
          .as[JobInfo]
          .logError(e => s"Parsing payload failed: $e")
        result  <- app.updateJob(JobId(id), jobInfo)
        resp    <- result match {
          case Right(_) => Ok()
          case Left(e)  => NotFound(FailureResponse(e))
        }
      } yield resp
  }

  // DELETE /jobs/uuid
  private val deleteJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ DELETE -> Root / UUIDVar(id) =>
      for {
        result <- app.deleteJob(JobId(id))
        resp   <- result match {
          case Right(_) => Ok()
          case Left(e)  => NotFound(FailureResponse(e))
        }
      } yield resp
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
