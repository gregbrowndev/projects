package com.rockthejvm.jobsboard.unit.tests.commands

import cats.data.EitherT
import cats.effect.IO
import cats.effect.implicits.*
import cats.implicits.*

import com.rockthejvm.jobsboard.fixtures.JobFixture
import com.rockthejvm.jobsboard.unit.UnitSpec

class UpdateJobInfoSpec extends UnitSpec with JobFixture {

  "UpdateJobInfoCommand" - {
    "should update jobInfo" in withAppContainer { container =>
      val jobService = container.core.services.jobs
      val resultIO   =
        for
          jobId <- EitherT(jobService.createJob(createAwesomeJobCommand))
          _     <- EitherT(jobService.updateJobInfo(updateJobInfoCommand))
          job   <- EitherT(jobService.get(jobId))
        yield job

      for
        result    <- resultIO.value
        assertion <- result match
          case Left(e)    => fail(s"Unexpected error: $e")
          case Right(job) => IO(job shouldBe updatedAwesomeJob)
      yield assertion
    }

    "should fail to update jobInfo and return an error" in withAppContainer {
      container =>
        pending // TODO
        ???
    }
  }
}
