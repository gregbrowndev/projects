package com.rockthejvm.jobsboard.unit.tests.commands

import cats.effect.IO
import com.softwaremill.diffx.scalatest.DiffShouldMatcher.*

import com.rockthejvm.jobsboard.fixtures.JobFixture
import com.rockthejvm.jobsboard.unit.UnitSpec

class CreateJobSpec extends UnitSpec with JobFixture {

  "CreateJobCommand" - {
    "should create a job and return its id" in withAppContainer { container =>
      val jobService = container.core.services.jobs

      // TODO - convert this to EitherT impl. (see UpdateJobInfoSpec)
      for {
        createJobResult <- jobService.createJob(createAwesomeJobCommand)
        assertion       <- createJobResult match
          case Left(error)  => fail(error)
          case Right(jobId) =>
            for
              jobResult <- jobService.get(jobId)
              assertion <- jobResult match
                case Left(error) => fail(error)
                case Right(job)  => IO(job shouldBe (awesomeJob))
            yield assertion
      } yield assertion
    }

    "should fail to create a job and return an error" in withAppContainer {
      container =>
        pending // TODO
        val jobService = container.core.services.jobs
        for createJobResult <- jobService.createJob(createInvalidJob)
        yield createJobResult shouldBe Left("TODO - add domain error")
    }
  }
}
