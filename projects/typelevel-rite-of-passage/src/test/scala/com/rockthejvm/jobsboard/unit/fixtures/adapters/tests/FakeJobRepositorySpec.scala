package com.rockthejvm.jobsboard.unit.fixtures.adapters.tests

import java.time.LocalDateTime
import scala.concurrent.*

import cats.data.EitherT
import cats.effect.*
import cats.effect.implicits.*
import cats.implicits.*
import org.scalatest.compatible.Assertion

import com.rockthejvm.jobsboard.AppContainer
import com.rockthejvm.jobsboard.adapters.in.http.HttpApi
import com.rockthejvm.jobsboard.core.application.ports.out.JobRepository
import com.rockthejvm.jobsboard.core.domain.model.{DomainError, job as Domain}
import com.rockthejvm.jobsboard.fixtures.JobFixture
import com.rockthejvm.jobsboard.unit.UnitSpec
import com.rockthejvm.jobsboard.unit.fixtures.adapters.{
  FakeJobRepository,
  FakeTimeAdapter
}

class FakeJobRepositorySpec extends UnitSpec {

  def withJobRepo(
      testCode: FakeJobRepository[IO] => IO[Assertion]
  ): IO[Assertion] =
    val fakeJobRepoRes: Resource[IO, FakeJobRepository[IO]] =
      for jobRepo <- FakeJobRepository[IO]
      yield jobRepo
    fakeJobRepoRes.use(jobRepo => testCode(jobRepo))

  val jobInfo: Domain.JobInfo = Domain.JobInfo(
    company = "Awesome Company",
    position = Domain.Position(
      title = "Tech Lead",
      description = "An awesome job in Berlin",
      seniority = "Senior".some,
      remote = false
    ),
    location = Domain.Location(
      office = "Berlin",
      country = "Germany".some
    ),
    salary = Domain.Salary(
      salaryLo = 2000.some,
      salaryHi = 3000.some,
      currency = "EUR"
    ),
    meta = Domain.JobInfoMeta(
      externalUrl = "https://rockthejvm.com/awesomejob",
      image = None,
      tags = Some(List("scala", "scala-3", "cats")),
      other = None
    )
  )

  "FakeJobRepository" - {
    "should make job with monotonically increasing JobId" in withJobRepo {
      jobRepo =>
        for {
          jobId        <- jobRepo.nextIdentity()
          anotherJobId <- jobRepo.nextIdentity()
        } yield {
          jobId shouldBe Domain.JobId.fromString(
            "00000000-0000-0000-0000-000000000001"
          )
          anotherJobId shouldBe Domain.JobId.fromString(
            "00000000-0000-0000-0000-000000000002"
          )
        }

    }

    "should save job" in withJobRepo { jobRepo =>
      val result = for
        jobId  <- EitherT.liftF(jobRepo.nextIdentity())
        job     = Domain.Job(
          id = jobId,
          date = LocalDateTime.parse("2023-01-01T00:00:00"),
          ownerEmail = "greg@rockthejvm.com",
          active = false,
          jobInfo = jobInfo
        )
        _      <- jobRepo.save(job)
        result <- jobRepo.get(job.id)
      yield result

      val expectedJob = Domain.Job(
        id = Domain.JobId.fromString("00000000-0000-0000-0000-000000000001"),
        ownerEmail = "greg@rockthejvm.com",
        date = LocalDateTime.parse("2023-01-01T00:00:00"),
        active = false,
        jobInfo = jobInfo
      )

      for actual <- result.value
      yield actual shouldBe Either.right(expectedJob)
    }

    "should save updated job" in withJobRepo { jobRepo =>
      val result =
        for
          jobId     <- EitherT.liftF(jobRepo.nextIdentity())
          job        = Domain.Job(
            id = jobId,
            date = LocalDateTime.parse("2023-01-01T00:00:00"),
            ownerEmail = "greg@rockthejvm.com",
            active = true,
            jobInfo = jobInfo
          )
          _         <- jobRepo.save(job)
          updatedJob = job.copy(
            active = true,
            jobInfo = jobInfo.copy(company = "Another Company")
          )
          _         <- jobRepo.update(updatedJob)
          result    <- jobRepo.get(job.id)
        yield result

      val expectedJob = Domain.Job(
        id = Domain.JobId.fromString("00000000-0000-0000-0000-000000000001"),
        ownerEmail = "greg@rockthejvm.com",
        date = LocalDateTime.parse("2023-01-01T00:00:00"),
        active = true,
        jobInfo = jobInfo.copy(company = "Another Company")
      )

      for actual <- result.value
      yield actual shouldBe Either.right(expectedJob)
    }

    "should delete job" in withJobRepo { jobRepo =>
      val result =
        for
          jobId  <- EitherT.liftF(jobRepo.nextIdentity())
          job     = Domain.Job(
            id = jobId,
            date = LocalDateTime.parse("2023-01-01T00:00:00"),
            ownerEmail = "greg@rockthejvm.com",
            active = true,
            jobInfo = jobInfo
          )
          _      <- jobRepo.save(job)
          _      <- jobRepo.delete(job.id)
          result <- EitherT.liftF(jobRepo.all())
        yield result

      for actual <- result.value
      yield actual shouldBe Either.right(List())
    }

    "should fail to find non-existent job" in withJobRepo { jobRepo =>
      val jobId =
        Domain.JobId.fromString("00000000-0000-0000-0000-000000000001")

      val result =
        for result <- jobRepo.get(jobId)
        yield result

      for actual <- result.value
      yield actual shouldBe Either.left(
        DomainError.jobNotFound(jobId)
      )
    }

  }
}
