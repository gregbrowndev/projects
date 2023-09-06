package com.rockthejvm.jobsboard.unit.tests.commands

import cats.data.EitherT
import cats.effect.IO
import cats.effect.implicits.*
import cats.implicits.*

import com.rockthejvm.jobsboard.core.domain.DomainError
import com.rockthejvm.jobsboard.fixtures.JobFixture
import com.rockthejvm.jobsboard.unit.tests.UnitSpec

class UpdateJobInfoSpec extends UnitSpec with JobFixture {

  "UpdateJobInfoCommand" - {
    "should update jobInfo" in withAppContainer { container =>
      val app      = container.core.app
      val resultIO =
        for
          jobId <- EitherT(app.createJob(createAwesomeJobCommand))
          _     <- EitherT(app.updateJobInfo(updateJobInfoCommand))
          job   <- EitherT(app.findJob(jobId))
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
