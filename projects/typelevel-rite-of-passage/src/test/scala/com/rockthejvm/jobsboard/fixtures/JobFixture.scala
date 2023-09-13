package com.rockthejvm.jobsboard.fixtures

import java.time.LocalDateTime
import java.util.UUID

import com.rockthejvm.jobsboard.core.application.services.*

trait JobFixture {
  val notFoundJobId: String = "6ea79557-3112-4c84-a8f5-1d1e2c300948"

  val awesomeJobInfo: JobInfoDTO                = JobInfoDTO(
    company = "Awesome Company",
    title = "Tech Lead",
    description = "An awesome job in Berlin",
    seniority = Some("Senior"),
    remote = false,
    office = "Berlin",
    country = Some("Germany"),
    salaryLo = Some(2000),
    salaryHi = Some(3000),
    currency = "EUR",
    externalUrl = "https://rockthejvm.com/awesomejob",
    tags = Some(List("scala", "scala-3", "cats")),
    image = None,
    other = None
  )
  val createAwesomeJobCommand: CreateJobArgsDTO = CreateJobArgsDTO(
    ownerEmail = "greg@rockthejvm.com",
    jobInfo = awesomeJobInfo
  )
  val awesomeJobId: String                      = "00000000-0000-0000-0000-000000000001"
  val awesomeJob: JobDTO                        = JobDTO(
    id = awesomeJobId,
    date = LocalDateTime.parse("2023-01-01T00:00:00"),
    ownerEmail = "greg@rockthejvm.com",
    jobInfo = awesomeJobInfo
  )

  val createInvalidJob: CreateJobArgsDTO = CreateJobArgsDTO(
    ownerEmail = "",
    jobInfo = JobInfoDTO(
      company = "",
      title = "",
      description = "",
      seniority = None,
      remote = false,
      office = "",
      country = None,
      salaryLo = None,
      salaryHi = Some(3000),
      currency = "",
      externalUrl = "rockthejvm.com/awesomejob",
      image = None,
      tags = Some(List("scala", "scala-3", "cats")),
      other = None
    )
  )

  val updatedAwesomeJobInfo: JobInfoDTO          = JobInfoDTO(
    company = "Awesome Company (Spain Branch)",
    title = "Engineering Manager",
    description = "An awesome job in Barcelona",
    seniority = Some("Highest"),
    remote = false,
    office = "Barcelona",
    country = Some("Spain"),
    salaryLo = Some(2200),
    salaryHi = Some(3200),
    currency = "USD",
    externalUrl = "http://www.awesome.com",
    image = Some("http://www.awesome.com/logo.png"),
    tags = Some(List("scala", "scala-3", "zio")),
    other = Some("Some additional info")
  )
  val updateJobInfoCommand: UpdateJobInfoArgsDTO = UpdateJobInfoArgsDTO(
    jobId = awesomeJobId,
    jobInfo = updatedAwesomeJobInfo
  )
  val updatedAwesomeJob: JobDTO                  = awesomeJob.copy(
    jobInfo = updatedAwesomeJobInfo
  )

  val rockTheJvmNewJobInfo: JobInfoDTO             = JobInfoDTO(
    company = "RockTheJvm",
    title = "Technical Author",
    description = "For the glory of the RockTheJvm!",
    seniority = Some("High"),
    remote = true,
    office = "From remote",
    country = Some("Romania"),
    salaryLo = Some(2000),
    salaryHi = Some(3500),
    currency = "EUR",
    externalUrl = "https://rockthejvm.com/",
    image = None,
    tags =
      Some(List("scala", "scala-3", "cats", "akka", "spark", "flink", "zio")),
    other = None
  )
  val createRockTheJvmJobCommand: CreateJobArgsDTO =
    createAwesomeJobCommand.copy(
      jobInfo = rockTheJvmNewJobInfo
    )
  val rockTheJvmNewJob: JobDTO                     = awesomeJob.copy(
    id = "00000000-0000-0000-0000-000000000002",
    jobInfo = rockTheJvmNewJobInfo
  )

  val createAnotherAwesomeJobCommand: CreateJobArgsDTO =
    createAwesomeJobCommand.copy()
  val anotherAwesomeJobId: String                      = "00000000-0000-0000-0000-000000000003"
  val anotherAwesomeJob: JobDTO                        =
    awesomeJob.copy(id = anotherAwesomeJobId)
}
