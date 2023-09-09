package com.rockthejvm.playground

import java.time.LocalDateTime
import scala.io.StdIn

import cats.Applicative.*
import cats.data.EitherT
import cats.effect.kernel.Resource
import cats.effect.{IO, IOApp}
import cats.implicits.*
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts

import com.rockthejvm.jobsboard.adapters.out.db.LiveJobRepository
import com.rockthejvm.jobsboard.adapters.out.time.LiveTimeAdapter
import com.rockthejvm.jobsboard.core.application.services.impl.LiveJobService
import com.rockthejvm.jobsboard.core.application.services.{CreateJobArgsDTO, JobInfoDTO, UpdateJobInfoArgsDTO}
import com.rockthejvm.jobsboard.core.domain.model.job.{
  Job,
  JobInfo,
  JobInfoMeta,
  Location,
  Position,
  Salary
}

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

  // Run with: sbt "runMain com.rockthejvm.playground.JobsPlayground"
  override def run: IO[Unit] =
//    val jobServiceResource = for {
//      xa          <- makePostgresResource()
//      timeAdapter <- LiveTimeAdapter[IO]
//      jobRepo     <- LiveJobRepository[IO](xa)
//      jobService  <- LiveJobService[IO](jobRepo, timeAdapter)
//    } yield jobService
//
//    jobServiceResource.use { jobService =>
//      val createJobArgsDTO = CreateJobArgsDTO(
//        ownerEmail = "gregbrowndev@gmail.com",
//        jobInfo = JobInfoDTO(
//          company = "Rock the JVM",
//          title = "Senior Engineer",
//          description = "10x developer",
//          remote = false,
//          seniority = Some("Senior"),
//          office = "London",
//          country = Some("UK"),
//          salaryLo = None,
//          salaryHi = Some(80000),
//          currency = "GBP",
//          externalUrl = "www.rockthejvm.com/jobs/1234",
//          image = None,
//          tags = None,
//          other = None
//        )
//      )
//
//      for {
//        createJobResult <- EitherT.liftF(jobService.createJob(createJobArgsDTO))
//        _               <- EitherT {
//          createJobResult.fold(
//            error => IO(Left(error)),
//            job => IO(Right(job))
//          )
//        }
//      } yield ()
//    }
    IO.unit
}
