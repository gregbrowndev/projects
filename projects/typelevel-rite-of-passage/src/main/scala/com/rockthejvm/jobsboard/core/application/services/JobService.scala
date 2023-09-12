package com.rockthejvm.jobsboard.core.application.services

import java.time.LocalDateTime

import cats.effect.kernel.Sync

import com.rockthejvm.jobsboard.core.application.services.pagination.PaginationDTO

trait JobService[F[_]: Sync] {

  // commands

  def createJob(args: CreateJobArgsDTO): F[CreateJobResponseDTO]

  def updateJobInfo(args: UpdateJobInfoArgsDTO): F[UpdateJobInfoResponseDTO]

  def deleteJob(args: DeleteJobArgsDTO): F[DeleteJobResponseDTO]

  // queries

  def get(id: String): F[Either[String, JobDTO]]

  def find(filter: JobFilterDTO, pagination: PaginationDTO): F[List[JobDTO]]
}

case class JobDTO(
    id: String,
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
type CreateJobResponseDTO = Either[String, String]

final case class UpdateJobInfoArgsDTO(jobId: String, jobInfo: JobInfoDTO)
type UpdateJobInfoResponseDTO = Either[String, Unit]

final case class DeleteJobArgsDTO(jobId: String)
type DeleteJobResponseDTO = Either[String, Unit]

final case class JobFilterDTO(
    companies: Option[List[String]] = None,
    locations: Option[List[String]] = None,
    countries: Option[List[String]] = None,
    seniorities: Option[List[String]] = None,
    tags: Option[List[String]] = None,
    maxSalary: Option[Int] = None,
    remote: Option[Boolean] = None
)
