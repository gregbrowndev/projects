package com.rockthejvm.jobsboard.http.routes

import java.util.UUID
import scala.collection.mutable

import cats.implicits.*
import cats.effect.Concurrent
import io.circe.generic.auto.*
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router

import com.rockthejvm.jobsboard.domain.job.Job
import com.rockthejvm.jobsboard.http.responses.FailureResponse
import com.rockthejvm.jobsboard.domain.job.JobInfo
import cats.MonadThrow

class JobRoutes[F[_]: Concurrent] extends Http4sDsl[F] {

  // In-memory dummy database
  private val database = mutable.Map[UUID, Job]()

  // POST /jobs?offset=x&limit=y { filters } // TODO add query params and filters
  private val allJobsRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case POST -> Root =>
      Ok(database.values)
  }

  // GET /jobs/uuid
  private val findJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / UUIDVar(id) =>
      Ok(s"TODO find job for $id")
  }

  // POST /jobs { jobInfo }
  private def createJob(jobInfo: JobInfo): F[Job] =
    Job(
      id = UUID.randomUUID(),
      date = System.currentTimeMillis(),
      ownerEmail = "TODO@rockthejvm.com",
      jobInfo = jobInfo,
      active = true
    ).pure[F]

  private val createJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "create" =>
      for {
        jobInfo <- req.as[JobInfo]
        job     <- createJob(jobInfo)
        resp    <- Created(job.id)
      } yield resp
  }

  // PUT /jobs/uuid { jobInfo }
  private val updateJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ PUT -> Root / UUIDVar(id) =>
      database.get(id) match {
        case Some(job) =>
          for {
            jobInfo <- req.as[JobInfo]
            _       <- database.put(id, job.copy(jobInfo = jobInfo)).pure[F]
            resp    <- Ok()
          } yield resp
        case None => NotFound(FailureResponse(s"Job $id not found"))
      }
  }

  // DELETE /jobs/uuid
  private val deleteJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ DELETE -> Root / UUIDVar(id) =>
      database.get(id) match {
        case Some(job) =>
          for {
            _    <- database.remove(id).pure[F]
            resp <- Ok()
          } yield resp
        case None => NotFound(FailureResponse(s"Job $id not found"))
      }
  }

  val routes = Router(
    "/jobs" -> (
      allJobsRoute
        <+> findJobRoute
        <+> createJobRoute
        <+> updateJobRoute
        <+> deleteJobRoute
    )
  )
}

object JobRoutes {
  def apply[F[_]: Concurrent] = new JobRoutes[F]
}
