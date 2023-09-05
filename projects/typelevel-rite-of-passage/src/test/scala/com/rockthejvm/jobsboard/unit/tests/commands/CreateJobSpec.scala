package com.rockthejvm.jobsboard.unit.tests.commands

import cats.effect.IO

import com.rockthejvm.jobsboard.fixtures.JobFixture
import com.rockthejvm.jobsboard.unit.tests.UnitSpec

class CreateJobSpec extends UnitSpec with JobFixture {

  "CreateJobCommand" - {
    "should create a job and return its id" in withAppContainer { container =>
      val app = container.core.app
      // TODO - convert this to EitherT impl. (see UpdateJobInfoSpec)
      for {
        jobCreated <- app.createJob(createAwesomeJobCommand)
        assertion  <- jobCreated match
          case Left(error)  => fail(error)
          case Right(jobId) =>
            for
              jobResult <- app.findJob(jobId)
              assertion <- jobResult match
                case Left(error) => fail(error.message)
                case Right(job)  => IO(job shouldMatchTo (awesomeJob))
            yield assertion
      } yield assertion
    }

    "should fail to create a job and return an error" in withAppContainer {
      container =>
        pending // TODO
        val app = container.core.app
        for jobCreated <- app.createJob(createInvalidJob)
        yield jobCreated shouldMatchTo Left("TODO - add domain error")
    }
  }
}
