package com.rockthejvm.jobsboard.unit.tests.queries

import java.util.UUID

import cats.data.EitherT
import cats.effect.IO

import com.rockthejvm.jobsboard.fixtures.JobFixture
import com.rockthejvm.jobsboard.unit.tests.UnitSpec

class FindJobQuerySpec extends UnitSpec with JobFixture {
  "FindJobQuery" - {
    "should return a job given its ID" in withAppContainer { container =>
      val jobService = container.core.services.jobs

      val resultIO =
        for
          jobId <- EitherT(jobService.createJob(createAwesomeJobCommand))
          job   <- EitherT(jobService.get(jobId))
        yield job

      for
        result    <- resultIO.value
        assertion <- result match
          case Left(e)    => fail(s"Unexpected error: $e")
          case Right(job) => IO(job shouldBe awesomeJob)
      yield assertion
    }

    "should return an error when job is not found" in withAppContainer {
      container =>
        val jobService = container.core.services.jobs

        val resultIO =
          for job <- EitherT(jobService.get(UUID.randomUUID().toString))
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
