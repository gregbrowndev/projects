package com.rockthejvm.jobsboard.e2e.tests

import java.util.UUID

import cats.effect.*
import cats.effect.implicits.*
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.*
import io.circe.Decoder
import io.circe.generic.auto.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.dsl.*
import org.http4s.implicits.*
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import com.rockthejvm.jobsboard.adapters.in.http.HttpApi
import com.rockthejvm.jobsboard.core.application.services.*
import com.rockthejvm.jobsboard.e2e.E2eSpec
import com.rockthejvm.jobsboard.fixtures.JobFixture

class CreateJobJourneySpec extends E2eSpec with JobFixture {

  given jobFilterQueryParamEncoder: QueryParamEncoder[JobFilterDTO] =
    QueryParamEncoder[String].contramap(_.asJson.noSpaces)

  "CreateJob" - {
    "should allow user to create a job and query for it" in withClient {
      client =>
        for {
          jobId <- client.expect[UUID](
            Request[IO](
              method = Method.POST,
              uri = Uri.unsafeFromString("/api/jobs/createJob")
            ).withEntity(createAwesomeJobCommand.asJson)
          )

          _ = jobId should not be null

          job <- client.expect[JobDTO](
            Request[IO](
              method = Method.GET,
              uri = Uri.unsafeFromString(s"/api/jobs/${jobId}")
            )
          )

          _ = job shouldBe awesomeJob

          jobsList1 <- client.expect[List[JobDTO]](
            Request[IO](
              method = Method.GET,
              uri = Uri
                .unsafeFromString("/api/jobs")
                .withQueryParam("filter", JobFilterDTO(remote = false.some))
                .withQueryParam("offset", 0)
                .withQueryParam("limit", 10)
            )
          )

          _ = jobsList1 should contain(awesomeJob)

          jobsList2 <- client.expect[List[JobDTO]](
            Request[IO](
              method = Method.GET,
              uri = Uri
                .unsafeFromString("/api/jobs")
                .withQueryParam("filter", JobFilterDTO(remote = true.some))
                .withQueryParam("offset", 0)
                .withQueryParam("limit", 10)
            )
          )

          _ = jobsList2 should be(empty)

        } yield succeed
    }
  }
}
