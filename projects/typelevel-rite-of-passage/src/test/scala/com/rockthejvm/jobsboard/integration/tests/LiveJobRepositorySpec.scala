package com.rockthejvm.jobsboard.integration.tests

import java.time.LocalDateTime

import cats.data.EitherT
import cats.effect.IO
import cats.effect.implicits.*
import cats.effect.kernel.Resource
import cats.implicits.*
import cats.syntax.either.*
import org.scalatest.compatible.Assertion
import org.scalatest.matchers.should.Matchers

import com.rockthejvm.jobsboard.adapters.out.db.LiveJobRepository
import com.rockthejvm.jobsboard.core.domain.model.job as Domain
import com.rockthejvm.jobsboard.integration.Fixture

class LiveJobRepositorySpec extends IntegrationSpec {

  def withLiveJobRepo(
      testCode: LiveJobRepository[IO] => IO[Assertion]
  ): IO[Assertion] =
    Fixture.liveJobRepositoryResource.use(jobRepo => testCode(jobRepo))

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

  "LiveJobRepository" - {
    "should make job with a unique JobId" in withLiveJobRepo { jobRepo =>
      for
        jobId        <- jobRepo.nextIdentity()
        anotherJobId <- jobRepo.nextIdentity()
      yield jobId should not be anotherJobId
    }

    "should save job" in withLiveJobRepo { jobRepo =>
      val resultT: EitherT[IO, String, (Domain.Job, Domain.Job)] =
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
          result <- jobRepo.get(job.id)
        yield (job, result)

      for result <- resultT.value
      yield result match {
        case Right((expected, actual)) => actual shouldBe expected
        case Left(error)               => fail(s"Unexpected error: $error")
      }
    }

    /* "should save updated job" in withLiveJobRepo { jobRepo => val result =
     * for job <- EitherT.liftF( jobRepo.make( ownerEmail =
     * "greg@rockthejvm.com", jobInfo = jobInfo ) ) _ <- jobRepo.create(job)
     * updatedJob = job.copy(active = true) _ <- jobRepo.update(updatedJob)
     * result <- jobRepo.find(job.id) yield result
     *
     * val expectedJob = Domain.Job( id =
     * Domain.JobId.fromString("00000000-0000-0000-0000-000000000001"),
     * ownerEmail = "greg@rockthejvm.com", date =
     * LocalDateTime.parse("2023-01-01T00:00:00"), active = true, jobInfo =
     * jobInfo )
     *
     * for actual <- result.value yield actual shouldBe
     * Either.right(expectedJob) }
     *
     * "should delete job" in withLiveJobRepo { jobRepo => val result =
     * for job <- EitherT.liftF( jobRepo.make( ownerEmail =
     * "greg@rockthejvm.com", jobInfo = jobInfo ) ) _ <- jobRepo.create(job) _
     * <- jobRepo.delete(job.id) result <- EitherT.liftF(jobRepo.all()) yield
     * result
     *
     * for actual <- result.value yield actual shouldBe Either.right(List()) }
     *
     * "should fail to find non-existent job" in withLiveJobRepo { jobRepo =>
     * val jobId =
     * Domain.JobId.fromString("00000000-0000-0000-0000-000000000001") val
     * result =
     * for result <- jobRepo.find(jobId) yield result
     *
     * for actual <- result.value yield actual shouldBe Either.left(
     * DomainError.JobNotFound(jobId) ) } */
  }
}
