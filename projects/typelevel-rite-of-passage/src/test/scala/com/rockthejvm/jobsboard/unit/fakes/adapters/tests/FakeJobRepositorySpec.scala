package com.rockthejvm.jobsboard.unit.fakes.adapters.tests

import java.time.LocalDateTime
import scala.concurrent.*

import cats.data.EitherT
import cats.effect.*
import cats.effect.implicits.*
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.*
import org.scalatest.Timer
import org.scalatest.compatible.Assertion
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
import com.rockthejvm.jobsboard.core.domain.job.{Job, JobId, JobInfo}
import com.rockthejvm.jobsboard.core.domain.job as Domain
import com.rockthejvm.jobsboard.fixtures.JobFixture
import com.rockthejvm.jobsboard.unit.fakes.adapters.{
  FakeJobRepository,
  FakeTimeAdapter
}

class FakeJobRepositorySpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers {

//  implicit lazy val ec: TestContext      = TestContext.apply()
//  implicit lazy val cs: ContextShift[IO] = IO.contextShift(ec)
//  implicit lazy val ioTimer: Timer[IO]   = ec.timer[IO]

  def withJobRepo(
      testCode: FakeJobRepository[IO] => IO[Assertion]
  ): IO[Assertion] =
    val jobRepoResource = for
      timeAdapter <- FakeTimeAdapter[IO]
      jobRepo     <- FakeJobRepository[IO](timeAdapter)
    yield jobRepo

    jobRepoResource.use(jobRepo => testCode(jobRepo))

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
    "should make job with monotonically increasing JobId" in withJobRepo {
      jobRepo =>
        for {
          job        <- jobRepo.make(
            ownerEmail = "greg@rockthejvm.com",
            jobInfo = jobInfo
          )
          anotherJob <- jobRepo.make(
            ownerEmail = "greg@rockthejvm.com",
            jobInfo = jobInfo
          )
        } yield {
          job.id shouldBe Domain.JobId.fromString(
            "00000000-0000-0000-0000-000000000001"
          )
          anotherJob.id shouldBe Domain.JobId.fromString(
            "00000000-0000-0000-0000-000000000002"
          )
        }

    }

    "should save job" in withJobRepo { jobRepo =>
      val result =
        for
          job     <- EitherT.liftF(
            jobRepo.make(
              ownerEmail = "greg@rockthejvm.com",
              jobInfo = jobInfo
            )
          )
          created <- jobRepo.create(job)
          result  <- jobRepo.find(job.id)
        yield result

      val expectedJob = Job(
        id = JobId.fromString("00000000-0000-0000-0000-000000000001"),
        ownerEmail = "greg@rockthejvm.com",
        date = LocalDateTime.parse("2023-01-01T00:00:00"),
        active = false,
        jobInfo = jobInfo
      )

      for actual <- result.value
      yield actual shouldBe Either.right(expectedJob)
    }

  }

  // TODO - add tests
  //  should save updated job (use jobRepo.all())
  //  should delete job
  //  should fail to find job that doesn't exist
}
