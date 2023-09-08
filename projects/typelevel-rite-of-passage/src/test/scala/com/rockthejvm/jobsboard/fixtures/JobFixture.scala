package com.rockthejvm.jobsboard.fixtures

import java.time.LocalDateTime
import java.util.UUID

import cats.syntax.all.*

import com.rockthejvm.jobsboard.core.application.ports.in.{Command, ViewModel}

trait JobFixture {
  val notFoundJobId: UUID =
    UUID.fromString("6ea79557-3112-4c84-a8f5-1d1e2c300948")

  val awesomeJobInfo: ViewModel.JobInfo          = ViewModel.JobInfo(
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
  val createAwesomeJobCommand: Command.CreateJob = Command.CreateJob(
    ownerEmail = "greg@rockthejvm.com",
    jobInfo = awesomeJobInfo
  )
  val awesomeJobId: UUID                         =
    UUID.fromString("00000000-0000-0000-0000-000000000001")
  val awesomeJob: ViewModel.Job                  = ViewModel.Job(
    id = awesomeJobId,
    date = LocalDateTime.parse("2023-01-01T00:00:00"),
    ownerEmail = "greg@rockthejvm.com",
    jobInfo = awesomeJobInfo
  )

  val createInvalidJob: Command.CreateJob = Command.CreateJob(
    ownerEmail = "",
    jobInfo = ViewModel.JobInfo(
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

  val updatedAwesomeJobInfo: ViewModel.JobInfo    = ViewModel.JobInfo(
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
  val updateJobInfoCommand: Command.UpdateJobInfo = Command.UpdateJobInfo(
    jobId = awesomeJobId,
    jobInfo = updatedAwesomeJobInfo
  )
  val updatedAwesomeJob: ViewModel.Job            = awesomeJob.copy(
    jobInfo = updatedAwesomeJobInfo
  )

  val rockTheJvmNewJobInfo: ViewModel.JobInfo       = ViewModel.JobInfo(
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
  val createRockTheJvmJobCommand: Command.CreateJob =
    createAwesomeJobCommand.copy(
      jobInfo = rockTheJvmNewJobInfo
    )
  val rockTheJvmNewJob: ViewModel.Job               = awesomeJob.copy(
    id = UUID.fromString("00000000-0000-0000-0000-000000000002"),
    jobInfo = rockTheJvmNewJobInfo
  )

  val createAnotherAwesomeJobCommand: Command.CreateJob =
    createAwesomeJobCommand.copy()
  val anotherAwesomeJobId: UUID                         =
    UUID.fromString("00000000-0000-0000-0000-000000000003")
  val anotherAwesomeJob: ViewModel.Job                  =
    awesomeJob.copy(id = anotherAwesomeJobId)
}
