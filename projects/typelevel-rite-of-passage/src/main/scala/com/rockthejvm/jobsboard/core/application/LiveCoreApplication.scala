package com.rockthejvm.jobsboard.core.application

import cats.Applicative.*
import cats.data.EitherT
import cats.effect.implicits.*
import cats.effect.kernel.{Async, Resource}
import cats.implicits.*
import cats.syntax.all.*
import cats.{Applicative, Monad}

import com.rockthejvm.jobsboard.core.domain.job.{Job, JobId, JobInfo}
import com.rockthejvm.jobsboard.syntax.*

import ports.in.CoreApplication
import ports.out.JobRepository

class LiveCoreApplication[F[_]: Async] private (
    val jobRepo: JobRepository[F]
) extends CoreApplication[F] {

  override def allJobs(): F[List[Job]] = jobRepo.all()

  override def createJob(jobInfo: JobInfo): F[Either[String, JobId]] =
    val jobIO = jobRepo.make(
      ownerEmail = "TODO@rockthejvm.com",
      jobInfo = jobInfo
    )
    for
      job <- EitherT.liftF(jobIO)
      _   <- jobRepo.create(job) // TODO errors ignored?
    yield job.id

  override def findJob(id: JobId): F[Either[String, Job]] =
    jobRepo.find(id)

  override def updateJob(
      id: JobId,
      jobInfo: JobInfo
  ): F[Either[String, Unit]] =
    for {
      job       <- jobRepo.find(id)
      updatedJob = job.copy(jobInfo = jobInfo)
      result    <- jobRepo.update(updatedJob)
    } yield result

  override def deleteJob(id: JobId): F[Either[String, Unit]] =
    for {
      job    <- jobRepo.find(id)
      result <- jobRepo.delete(job.id)
    } yield result
}

object LiveCoreApplication {
  def apply[F[_]: Async](
      jobRepo: JobRepository[F]
  ): Resource[F, LiveCoreApplication[F]] =
    Resource.pure(new LiveCoreApplication[F](jobRepo))
}
