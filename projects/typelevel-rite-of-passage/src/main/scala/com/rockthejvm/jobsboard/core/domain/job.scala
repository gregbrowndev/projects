package com.rockthejvm.jobsboard.core.domain

import java.time.LocalDateTime
import java.util.UUID

object job {
  case class JobId(value: java.util.UUID)
  object JobId {
    // TODO - add error handling
    def fromString(value: String): JobId = JobId(UUID.fromString(value))
  }

  case class Job(
      id: JobId,
      date: LocalDateTime,
      ownerEmail: String,
      active: Boolean = false,
      jobInfo: JobInfo
  )

  case class JobInfo(
      company: String,
      position: Position,
      location: Location,
      salary: Salary,
      meta: JobInfoMeta
  )

  case class Position(
      title: String,
      description: String,
      seniority: Option[String],
      remote: Boolean
  )

  case class Location(
      office: String,
      country: Option[String]
  )

  case class Salary(
      salaryLo: Option[Int],
      salaryHi: Option[Int],
      currency: String
  )

  case class JobInfoMeta(
      externalUrl: String,
      image: Option[String],
      tags: Option[List[String]],
      other: Option[String]
  )

  object JobInfo {
    val empty: JobInfo =
      // TODO - this should result in domain errors
      JobInfo(
        "",
        Position("", "", None, false),
        Location("", None),
        Salary(None, None, ""),
        JobInfoMeta("", None, None, None)
      )
  }
}
