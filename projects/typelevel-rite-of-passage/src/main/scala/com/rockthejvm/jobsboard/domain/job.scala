package com.rockthejvm.jobsboard.domain

import java.util.UUID

object job {
  case class Job(
      id: UUID,
      date: Long,
      ownerEmail: String,
      jobInfo: JobInfo,
      active: Boolean = false
  )

  case class JobInfo(
      company: String,
      position: Position,
      location: Location,
      salary: Option[Salary],
      jobInfoMeta: JobInfoMeta
  )

  case class Position(
      title: String,
      description: String,
      seniority: Option[String]
  )

  case class Salary(
      salaryLo: Option[Int],
      salaryHi: Int,
      currency: String
  )

  case class Location(
      office: String,
      remote: Boolean,
      country: Option[String]
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
        Position("", "", None),
        Location("", false, None),
        None,
        JobInfoMeta("", None, None, None)
      )
  }
}
