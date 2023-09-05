package com.rockthejvm.jobsboard.unit.tests.queries

import java.util.UUID

import cats.data.EitherT
import cats.effect.IO

import com.rockthejvm.jobsboard.core.domain.DomainError
import com.rockthejvm.jobsboard.fixtures.JobFixture
import com.rockthejvm.jobsboard.unit.tests.UnitSpec

class FindJobQuerySpec extends UnitSpec with JobFixture {
  "FindJobQuery" - {
    "should return a job given its ID" in withAppContainer { container =>
      val app      = container.core.app
      val resultIO =
        for
          jobId <- EitherT(app.createJob(createAwesomeJobCommand))
          job   <- EitherT(app.findJob(jobId))
        yield job

      for
        result    <- resultIO.value
        assertion <- result match
          case Left(e: DomainError.JobNotFound) => fail(e.message)
          case Left(e)                          => fail(s"Unexpected error: $e")
          case Right(job)                       => IO(job shouldBe awesomeJob)
      yield assertion
    }

    "should return an error when job is not found" in withAppContainer {
      container =>
        val app      = container.core.app
        val resultIO =
          for job <- EitherT(app.findJob(UUID.randomUUID()))
          yield job

        for
          result    <- resultIO.value
          assertion <- result match
            case Left(error) => IO(succeed)
            case Right(job)  => fail(s"A job was found: $job")
        yield assertion
    }
  }
}
