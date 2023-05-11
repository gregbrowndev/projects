package com.rockthejvm.jobsboard.unit.fakes.adapters.tests

import cats.data.EitherT
import cats.effect.*
import cats.effect.implicits.*
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.*
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

import com.rockthejvm.jobsboard.AppContainer
import com.rockthejvm.jobsboard.adapters.in.http.HttpApi
import com.rockthejvm.jobsboard.core.application.ports.in.{
  Command,
  CoreApplication,
  ViewModel
}
import com.rockthejvm.jobsboard.core.application.ports.out.JobRepository
import com.rockthejvm.jobsboard.core.domain.job as Domain
import com.rockthejvm.jobsboard.fixtures.JobFixture
import com.rockthejvm.jobsboard.unit.fakes.adapters.FakeJobRepository

class JobRoutesSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {

  val jobRepoResource: Resource[IO, FakeJobRepository[IO]] =
    FakeJobRepository[IO]

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

  "FakeJobRepository" - {
    "should make job with monotonically increasing JobId" in {
      jobRepoResource.use { jobRepo =>
        for {
          job <- jobRepo.make(
            ownerEmail = "greg@rockthejvm.com",
            jobInfo = jobInfo
          )
        } yield job.id shouldBe Domain.JobId.fromString(
          "00000000-0000-0000-0000-000000000001"
        )
      }
    }

    "should save job" in {
      jobRepoResource.use { jobRepo =>
        val jobIO  = jobRepo.make(
          ownerEmail = "greg@rockthejvm.com",
          jobInfo = jobInfo
        )
        val result = for
          job     <- EitherT.liftF(jobIO)
          created <- jobRepo.create(job)
        yield
          created shouldBe ()
          jobRepo.all() shouldBe List(job)

        // Unwrap Either result
        result.fold(
          left => throw new RuntimeException("Something went wrong"),
          right => right
        )
      }
    }
  }
}
