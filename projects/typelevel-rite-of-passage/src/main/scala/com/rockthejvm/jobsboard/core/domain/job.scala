package com.rockthejvm.jobsboard.core.domain

import java.util.UUID
import java.time.LocalDateTime

object job {
  case class Job(
      id: UUID,
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
