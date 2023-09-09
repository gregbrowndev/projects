package com.rockthejvm.jobsboard.core.application.services

import java.time.LocalDateTime
import java.util.UUID

import cats.effect.kernel.Sync

case class JobDTO(
    id: UUID,
    date: LocalDateTime,
    ownerEmail: String,
    active: Boolean = false,
    jobInfo: JobInfoDTO
)

case class JobInfoDTO(
    company: String,
    title: String,
    description: String,
    seniority: Option[String],
    remote: Boolean,
    office: String,
    country: Option[String],
    salaryLo: Option[Int],
    salaryHi: Option[Int],
    currency: String,
    externalUrl: String,
    image: Option[String],
    tags: Option[List[String]],
    other: Option[String]
)

final case class CreateJobArgsDTO(
    ownerEmail: String,
    jobInfo: JobInfoDTO
)
type CreateJobResponseDTO = Either[String, UUID]

final case class UpdateJobInfoArgsDTO(jobId: UUID, jobInfo: JobInfoDTO)
type UpdateJobInfoResponseDTO = Either[String, Unit]

final case class DeleteJobArgsDTO(jobId: UUID)
type DeleteJobResponseDTO = Either[String, Unit]

trait JobService[F[_]: Sync] {

  // commands

  def createJob(args: CreateJobArgsDTO): F[CreateJobResponseDTO]

  def updateJobInfo(args: UpdateJobInfoArgsDTO): F[UpdateJobInfoResponseDTO]

  def deleteJob(args: DeleteJobArgsDTO): F[DeleteJobResponseDTO]

  // queries

  def findJob(id: UUID): F[Either[String, JobDTO]]

  def allJobs(): F[List[JobDTO]]
}
