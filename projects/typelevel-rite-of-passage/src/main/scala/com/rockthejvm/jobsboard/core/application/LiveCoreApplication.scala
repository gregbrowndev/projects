package com.rockthejvm.jobsboard.core.application

import java.util.UUID

import cats.Applicative.*
import cats.data.EitherT
import cats.effect.implicits.*
import cats.effect.kernel.{Resource, Sync}
import cats.implicits.*
import cats.syntax.all.*
import cats.{Applicative, Monad}

import com.rockthejvm.jobsboard.core.application.ports.in.{Command, ViewModel}
import com.rockthejvm.jobsboard.core.domain.DomainError
import com.rockthejvm.jobsboard.core.domain.job.{Job, JobId, JobInfo}
import com.rockthejvm.jobsboard.syntax.*

import ports.in.CoreApplication
import ports.out.JobRepository

class LiveCoreApplication[F[_]: Sync] private (
    val jobRepo: JobRepository[F]
) extends CoreApplication[F] {
  // Commands

  override def createJob(cmd: Command.CreateJob): F[Either[String, UUID]] =
    val jobIO = jobRepo.make(
      ownerEmail = cmd.ownerEmail,
      jobInfo = ViewModel.JobInfo.toDomain(cmd.jobInfo)
    )
    for
      job <- EitherT.liftF(jobIO)
      _   <- jobRepo.create(job) // TODO errors ignored?
    yield job.id.value

  override def updateJobInfo(
      cmd: Command.UpdateJobInfo
  ): F[Either[String, Unit]] =
    val jobId   = JobId(cmd.jobId)
    val jobInfo = ViewModel.JobInfo.toDomain(cmd.jobInfo)
    for {
      job       <- jobRepo.find(jobId)
      updatedJob = job.copy(jobInfo = jobInfo)
      result    <- jobRepo.update(updatedJob)
    } yield result

  override def deleteJob(
      cmd: Command.DeleteJob
  ): F[Either[String, Unit]] =
    val jobId = JobId(cmd.jobId)
    for {
      job    <- jobRepo.find(jobId)
      result <- jobRepo.delete(job.id)
    } yield result

  // Queries
  override def allJobs(): F[List[ViewModel.Job]] =
    for jobList <- jobRepo.all()
    yield jobList.map(ViewModel.Job.fromDomain)

  override def findJob(
      id: UUID
  ): F[Either[String, ViewModel.Job]] =
    val jobId = JobId(id)
    for job <- jobRepo.find(jobId)
    yield ViewModel.Job.fromDomain(job)
}

object LiveCoreApplication {
  def apply[F[_]: Sync](
      jobRepo: JobRepository[F]
  ): Resource[F, LiveCoreApplication[F]] =
    Resource.pure(new LiveCoreApplication[F](jobRepo))
}
