package com.rockthejvm.playground

import java.time.LocalDateTime
import scala.io.StdIn

import cats.implicits.*
import cats.effect.IOApp
import cats.effect.IO
import cats.effect.kernel.Resource
import doobie.util.ExecutionContexts
import doobie.hikari.HikariTransactor

import com.rockthejvm.jobsboard.core.domain.job.{
  JobInfo,
  Position,
  Location,
  Salary,
  JobInfoMeta,
  Job
}
import com.rockthejvm.jobsboard.adapters.out.db.LiveJobRepository

object JobsPlayground extends IOApp.Simple {

  def makePostgresResource(): Resource[IO, HikariTransactor[IO]] = for {
    ce <- ExecutionContexts.fixedThreadPool[IO](32)
    xa <- HikariTransactor.newHikariTransactor[IO](
      "org.postgresql.Driver",                      // JDBC connector
      "jdbc:postgresql://localhost:5433/jobsboard", // connection string
      "docker",                                     // username
      "docker",                                     // password
      ce
    )
  } yield xa

  /*
  Run with: sbt "runMain com.rockthejvm.playground.JobsPlayground"
   */
  override def run: IO[Unit] =
    val jobRepoResource = for {
      xa      <- makePostgresResource()
      jobRepo <- LiveJobRepository[IO](xa)
    } yield jobRepo

    jobRepoResource.use { jobRepo =>
      for {
        job <- jobRepo.make(
          ownerEmail = "gregbrowndev@gmail.com",
          jobInfo = JobInfo(
            company = "Rock the JVM",
            position = Position(
              title = "Senior Engineer",
              description = "10x developer",
              remote = false,
              seniority = Some("Senior")
            ),
            location = Location(
              office = "London",
              country = Some("UK")
            ),
            salary = Some(
              Salary(
                salaryLo = None,
                salaryHi = 80000,
                currency = "GBP"
              )
            ),
            meta = JobInfoMeta(
              externalUrl = "www.rockthejvm.com/jobs/1234",
              image = None,
              tags = None,
              other = None
            )
          )
        )
        _       <- jobRepo.create(job)
        _       <- IO(println("Created job...")) *> IO(StdIn.readLine)
        allJobs <- jobRepo.all()
        _       <- IO(println(s"All jobs: $allJobs")) *> IO(StdIn.readLine)
        updatedJob = job.copy(active = true)
        _            <- jobRepo.update(updatedJob)
        _            <- IO(println("Updated job...")) *> IO(StdIn.readLine)
        myJob        <- jobRepo.find(id = job.id)
        _            <- IO(println(s"Your job: $myJob")) *> IO(StdIn.readLine)
        _            <- jobRepo.delete(id = job.id)
        _            <- IO(println("Deleted job...")) *> IO(StdIn.readLine)
        allJobsFinal <- jobRepo.all()
        _ <- IO(println(s"All jobs: $allJobsFinal")) *> IO(StdIn.readLine)

      } yield ()
    }
}
