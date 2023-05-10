package com.rockthejvm.jobsboard.core.domain

import java.time.LocalDateTime
import java.util.UUID

object job {
  case class JobId(value: java.util.UUID)

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
      salary: Option[Salary],
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
      salaryHi: Int,
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
      JobInfo(
        "",
        Position("", "", None, false),
        Location("", None),
        None,
        JobInfoMeta("", None, None, None)
      )
  }
}
