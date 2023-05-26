package com.rockthejvm.jobsboard.unit.tests.queries

import cats.data.EitherT
import cats.effect.IO

import com.rockthejvm.jobsboard.fixtures.JobFixture
import com.rockthejvm.jobsboard.unit.tests.UnitSpec

class AllJobsQuerySpec extends UnitSpec with JobFixture {
  "AllJobsQuery" - {
    "should return a list of jobs" in withAppContainer { container =>
      val app      = container.core.app
      val resultIO =
        for
          _    <- EitherT(app.createJob(createAwesomeJobCommand))
          jobs <- EitherT.liftF(app.allJobs())
        yield jobs

      for
        result    <- resultIO.value
        assertion <- result match
          case Left(error) => fail(error)
          case Right(jobs) => IO(jobs shouldMatchTo List(awesomeJob))
      yield assertion
    }

    "should return an empty list of jobs" in withAppContainer { container =>
      val app = container.core.app
      for
        jobs      <- app.allJobs()
        assertion <- IO(jobs shouldMatchTo List())
      yield assertion
    }
  }
}
