package com.rockthejvm.jobsboard.integration.tests

import java.time.LocalDateTime

import cats.data.EitherT
import cats.effect.IO
import cats.effect.implicits.*
import cats.effect.kernel.Resource
import cats.implicits.*
import org.scalatest.compatible.Assertion
import org.scalatest.matchers.should.Matchers

import com.rockthejvm.jobsboard.adapters.out.db.LiveJobRepository
import com.rockthejvm.jobsboard.core.domain.job as Domain
import com.rockthejvm.jobsboard.integration.Fixture

class LiveJobRepositorySpec extends IntegrationSpec {

  def withLiveJobRepo(
      testCode: LiveJobRepository[IO] => IO[Assertion]
  ): IO[Assertion] =
    Fixture.liveJobRepositoryResource.use(jobRepo => testCode(jobRepo))

  val jobInfo = Domain.JobInfo(
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

  "LiveJobRepository" - {
    "should make job with a unique JobId" in withLiveJobRepo { jobRepo =>
      for
        job        <- jobRepo.make(
          ownerEmail = "greg@rockthejvm.com",
          jobInfo = jobInfo
        )
        anotherJob <- jobRepo.make(
          ownerEmail = "greg@rockthejvm.com",
          jobInfo = jobInfo
        )
      yield job.id should not be anotherJob.id
    }


    "should save job" in withLiveJobRepo { jobRepo =>
      val result =
        for
          job    <- EitherT.liftF(
            jobRepo.make(
              ownerEmail = "greg@rockthejvm.com",
              jobInfo = jobInfo
            )
          )
          _      <- jobRepo.create(job)
          result <- jobRepo.find(job.id)
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

    "should save updated job" in withLiveJobRepo { jobRepo =>
      val result =
        for
          job       <- EitherT.liftF(
            jobRepo.make(
              ownerEmail = "greg@rockthejvm.com",
              jobInfo = jobInfo
            )
          )
          _         <- jobRepo.create(job)
          updatedJob = job.copy(active = true)
          _         <- jobRepo.update(updatedJob)
          result    <- jobRepo.find(job.id)
        yield result

      val expectedJob = Domain.Job(
        id = Domain.JobId.fromString("00000000-0000-0000-0000-000000000001"),
        ownerEmail = "greg@rockthejvm.com",
        date = LocalDateTime.parse("2023-01-01T00:00:00"),
        active = true,
        jobInfo = jobInfo
      )

      for actual <- result.value
      yield actual shouldBe Either.right(expectedJob)
    }

    "should delete job" in withLiveJobRepo { jobRepo =>
      val result =
        for
          job    <- EitherT.liftF(
            jobRepo.make(
              ownerEmail = "greg@rockthejvm.com",
              jobInfo = jobInfo
            )
          )
          _      <- jobRepo.create(job)
          _      <- jobRepo.delete(job.id)
          result <- EitherT.liftF(jobRepo.all())
        yield result

      for actual <- result.value
      yield actual shouldBe Either.right(List())
    }

    "should fail to find non-existent job" in withLiveJobRepo { jobRepo =>
      val result =
        for result <- jobRepo.find(
            Domain.JobId.fromString("00000000-0000-0000-0000-000000000001")
          )
        yield result

      for actual <- result.value
      yield actual shouldBe Either.left(
        "Job with ID '00000000-0000-0000-0000-000000000001' not found"
      )
    }
  }
}
  