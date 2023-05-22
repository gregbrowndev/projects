package com.rockthejvm.jobsboard.unit.tests

import cats.data.EitherT
import cats.effect.IO

import com.rockthejvm.jobsboard.core.application.ports.in.Command
import com.rockthejvm.jobsboard.fixtures.JobFixture

class DeleteJobSpec extends UnitSpec with JobFixture {
  "DeleteJobCommand" - {
    "should delete the job" in withAppContainer { container =>
      val app      = container.core.app
      val resultIO = for {
        jobId <- EitherT(app.createJob(createAwesomeJobCommand))
        _     <- EitherT(app.deleteJob(Command.DeleteJob(jobId = jobId)))
        job   <- EitherT(app.findJob(jobId))
      } yield job

      for
        result    <- resultIO.value
        assertion <- result match {
          case Left(_)    => IO(succeed)
          case Right(job) => fail("Job was not deleted")
        }
      yield assertion
    }
  }
}
