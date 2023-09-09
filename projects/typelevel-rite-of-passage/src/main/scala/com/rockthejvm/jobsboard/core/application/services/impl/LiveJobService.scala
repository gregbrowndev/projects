package com.rockthejvm.jobsboard.core.application.services.impl

import java.util.UUID

import cats.Applicative.*
import cats.data.EitherT
import cats.effect.implicits.*
import cats.effect.kernel.{Resource, Sync}
import cats.implicits.*
import cats.syntax.all.*
import cats.{Applicative, Monad}

import com.rockthejvm.jobsboard.core.application.ports.out.{
  JobRepository,
  TimeAdapter
}
import com.rockthejvm.jobsboard.core.application.services.*
import com.rockthejvm.jobsboard.core.domain.model.job.{
  Job,
  JobId,
  JobInfo,
  JobInfoMeta,
  Location,
  Position,
  Salary
}
import com.rockthejvm.jobsboard.syntax.*

class LiveJobService[F[_]: Sync] private (
    val jobRepo: JobRepository[F],
    val timeAdapter: TimeAdapter[F]
) extends JobService[F] {

  // Commands

  override def createJob(args: CreateJobArgsDTO): F[CreateJobResponseDTO] =
    val jobInfo = jobInfoFromDTO(args.jobInfo)
    for
      job <- EitherT.liftF(
        makeJob(
          ownerEmail = args.ownerEmail,
          jobInfo = jobInfo
        )
      )
      _   <- jobRepo.create(job) // TODO errors ignored?
    yield job.id.value

  override def updateJobInfo(
      args: UpdateJobInfoArgsDTO
  ): F[UpdateJobInfoResponseDTO] =
    val jobId   = JobId(args.jobId)
    val jobInfo = jobInfoFromDTO(args.jobInfo)
    for {
      job       <- jobRepo.find(jobId)
      updatedJob = job.copy(
        jobInfo = jobInfo
      )
      result    <- jobRepo.update(updatedJob)
    } yield result

  override def deleteJob(args: DeleteJobArgsDTO): F[DeleteJobResponseDTO] =
    val jobId = JobId(args.jobId)
    for {
      job    <- jobRepo.find(jobId)
      result <- jobRepo.delete(job.id)
    } yield result

  // Queries

  override def allJobs(): F[List[JobDTO]] =
    for jobList <- jobRepo.all()
    yield jobList.map(jobToDTO)

  override def findJob(
      id: UUID
  ): F[Either[String, JobDTO]] =
    val jobId = JobId(id)
    for job <- jobRepo.find(jobId)
    yield jobToDTO(job)

  // Factories

  private def makeJob(ownerEmail: String, jobInfo: JobInfo): F[Job] =
    for {
      id   <- jobRepo.nextIdentity()
      date <- timeAdapter.now()
    } yield Job(
      id = id,
      date = date,
      ownerEmail = ownerEmail,
      active = false,
      jobInfo = jobInfo
    )

  // Mappers

  private def jobInfoFromDTO(jobInfo: JobInfoDTO): JobInfo =
    JobInfo(
      company = jobInfo.company,
      position = Position(
        title = jobInfo.title,
        description = jobInfo.description,
        seniority = jobInfo.seniority,
        remote = jobInfo.remote
      ),
      location = Location(
        office = jobInfo.office,
        country = jobInfo.country
      ),
      salary = Salary(
        salaryLo = jobInfo.salaryLo,
        salaryHi = jobInfo.salaryHi,
        currency = jobInfo.currency
      ),
      meta = JobInfoMeta(
        externalUrl = jobInfo.externalUrl,
        image = jobInfo.image,
        tags = jobInfo.tags,
        other = jobInfo.other
      )
    )

  private def jobToDTO(job: Job): JobDTO =
    JobDTO(
      id = job.id.value,
      date = job.date,
      ownerEmail = job.ownerEmail,
      active = job.active,
      jobInfo = jobInfoToDTO(job.jobInfo)
    )

  private def jobInfoToDTO(jobInfo: JobInfo): JobInfoDTO =
    JobInfoDTO(
      company = jobInfo.company,
      title = jobInfo.position.title,
      description = jobInfo.position.description,
      seniority = jobInfo.position.seniority,
      remote = jobInfo.position.remote,
      office = jobInfo.location.office,
      country = jobInfo.location.country,
      salaryLo = jobInfo.salary.salaryLo,
      salaryHi = jobInfo.salary.salaryHi,
      currency = jobInfo.salary.currency,
      externalUrl = jobInfo.meta.externalUrl,
      image = jobInfo.meta.image,
      tags = jobInfo.meta.tags,
      other = jobInfo.meta.other
    )
}

object LiveJobService {
  def apply[F[_]: Sync](
      jobRepo: JobRepository[F],
      timeAdapter: TimeAdapter[F]
  ): Resource[F, LiveJobService[F]] =
    Resource.pure(
      new LiveJobService[F](jobRepo = jobRepo, timeAdapter = timeAdapter)
    )
}
