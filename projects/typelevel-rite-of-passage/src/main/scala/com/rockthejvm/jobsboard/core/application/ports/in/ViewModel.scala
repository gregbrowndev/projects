package com.rockthejvm.jobsboard.core.application.ports.in

import java.time.LocalDateTime
import java.util.UUID

import cats.implicits.*

import com.rockthejvm.jobsboard.core.domain.job as Domain

object ViewModel {
  type JobId = UUID

  case class Job(
      id: JobId,
      date: LocalDateTime,
      ownerEmail: String,
      active: Boolean = false,
      jobInfo: JobInfo
  )
  object Job     {
    def fromDomain(job: Domain.Job): Job =
      Job(
        id = job.id.value,
        date = job.date,
        ownerEmail = job.ownerEmail,
        active = job.active,
        jobInfo = JobInfo.fromDomain(job.jobInfo)
      )
    def toDomain(job: Job): Domain.Job   =
      Domain.Job(
        id = Domain.JobId(job.id),
        date = job.date,
        ownerEmail = job.ownerEmail,
        active = job.active,
        jobInfo = JobInfo.toDomain(job.jobInfo)
      )
  }

  case class JobInfo(
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
  object JobInfo {
    def fromDomain(jobInfo: Domain.JobInfo): JobInfo =
      JobInfo(
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

    def toDomain(jobInfo: JobInfo): Domain.JobInfo = {
      Domain.JobInfo(
        company = jobInfo.company,
        position = Domain.Position(
          title = jobInfo.title,
          description = jobInfo.description,
          seniority = jobInfo.seniority,
          remote = jobInfo.remote
        ),
        location = Domain.Location(
          office = jobInfo.office,
          country = jobInfo.country
        ),
        salary = Domain.Salary(
          salaryLo = jobInfo.salaryLo,
          salaryHi = jobInfo.salaryHi,
          currency = jobInfo.currency
        ),
        meta = Domain.JobInfoMeta(
          externalUrl = jobInfo.externalUrl,
          image = jobInfo.image,
          tags = jobInfo.tags,
          other = jobInfo.other
        )
      )
    }
  }
}
