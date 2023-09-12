package com.rockthejvm.jobsboard.unit.tests.queries

import cats.data.EitherT
import cats.effect.IO

import com.rockthejvm.jobsboard.core.application.services.JobFilterDTO
import com.rockthejvm.jobsboard.core.application.services.pagination.PaginationDTO
import com.rockthejvm.jobsboard.fixtures.JobFixture
import com.rockthejvm.jobsboard.unit.UnitSpec

class AllJobsQuerySpec extends UnitSpec with JobFixture {
  "AllJobsQuery" - {
    "should return a list of jobs" in withAppContainer { container =>
      val jobService = container.core.services.jobs

      val resultIO =
        for
          _    <- EitherT(jobService.createJob(createAwesomeJobCommand))
          jobs <- EitherT.liftF(
            jobService.find(
              JobFilterDTO(),
              PaginationDTO(0, 10)
            )
          )
        yield jobs

      for
        result    <- resultIO.value
        assertion <- result match
          case Left(error) => fail(error)
          case Right(jobs) => IO(jobs shouldBe List(awesomeJob))
      yield assertion
    }

    "should return an empty list of jobs" in withAppContainer { container =>
      val jobService = container.core.services.jobs

      for
        jobs      <- jobService.find(
          JobFilterDTO(),
          PaginationDTO(0, 10)
        )
        assertion <- IO(jobs shouldBe List())
      yield assertion
    }
  }
}
