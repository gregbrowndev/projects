package com.rockthejvm.jobsboard.unit.tests.commands

import cats.data.EitherT
import cats.effect.IO

import com.rockthejvm.jobsboard.core.application.services.DeleteJobArgsDTO
import com.rockthejvm.jobsboard.fixtures.JobFixture
import com.rockthejvm.jobsboard.unit.tests.UnitSpec

class DeleteJobSpec extends UnitSpec with JobFixture {

  "DeleteJobCommand" - {
    "should delete the job" in withAppContainer { container =>
      val jobService = container.core.services.jobs
      val resultIO   =
        for
          jobId <- EitherT(jobService.createJob(createAwesomeJobCommand))
          _     <- EitherT(jobService.deleteJob(DeleteJobArgsDTO(jobId = jobId)))
          job   <- EitherT(jobService.get(jobId))
        yield job

      for
        result    <- resultIO.value
        assertion <- result match
          case Left(_)    => IO(succeed)
          case Right(job) => fail("Job was not deleted")
      yield assertion
    }
  }
}
