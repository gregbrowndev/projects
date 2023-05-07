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
import com.rockthejvm.jobsboard.core.domain.job.{Job, JobInfo}
import com.rockthejvm.jobsboard.core.ports.JobRepository

class JobRoutes[F[_]: Concurrent: Logger] private (jobRepo: JobRepository[F])
    extends Http4sDsl[F] {

  // TODO - make this a GET and add filters to the URL. This is better as the
  // filters can be bookmarked and we can fix the other URL routes
  // POST /jobs?offset=x&limit=y { filters } // TODO add query params and filters
  private val allJobsRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case POST -> Root =>
      for {
        jobsList <- jobRepo.all()
        resp     <- Ok(jobsList)
      } yield resp
  }

  // POST /jobs/create { jobInfo }
  // We can test this route with:
  // http POST 'localhost:8080/api/jobs/create' < ./src/main/resources/payloads/createJob.json
  private val createJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "create" =>
      for {
        jobInfo <- req.as[JobInfo].logError(e => s"Parsing payload failed: $e")
        job <- jobRepo.make(
          ownerEmail = "TODO@rockthejvm.com",
          jobInfo = jobInfo
        )
        _    <- jobRepo.create(job)
        resp <- Created(job.id)
      } yield resp
  }

  // GET /jobs/uuid
  private val findJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / UUIDVar(id) =>
      jobRepo.find(id).flatMap {
        case Some(job) => Ok(job)
        case None      => NotFound(FailureResponse(s"Job $id not found"))
      }
  }

  // PUT /jobs/uuid { jobInfo }
  private val updateJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ PUT -> Root / UUIDVar(id) =>
      for {
        result <- jobRepo.find(id)
        resp <- result match {
          case Some(job) =>
            for {
              jobInfo <- req
                .as[JobInfo]
                .logError(e => s"Parsing payload failed: $e")
              updatedJob = job.copy(jobInfo = jobInfo)
              _    <- jobRepo.update(updatedJob)
              resp <- Ok()
            } yield resp
          case None => NotFound(FailureResponse(s"Job $id not found"))
        }
      } yield resp
  }

  // DELETE /jobs/uuid
  private val deleteJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ DELETE -> Root / UUIDVar(id) =>
      jobRepo.find(id).flatMap {
        case Some(job) =>
          for {
            _    <- jobRepo.delete(id)
            resp <- Ok()
          } yield resp
        case None => NotFound(FailureResponse(s"Job $id not found"))
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
  def apply[F[_]: Concurrent: Logger](jobRepo: JobRepository[F]) =
    new JobRoutes[F](jobRepo)
}
