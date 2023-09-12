package com.rockthejvm.jobsboard.e2e.journeys

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
import com.rockthejvm.jobsboard.fixtures.JobFixture
import com.rockthejvm.jobsboard.unit.fakes.FakeAppContainer
import com.rockthejvm.jobsboard.unit.tests.UnitSpec

class CreateJobJourneySpec extends UnitSpec with JobFixture {
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  given jobFilterQueryParamEncoder: QueryParamEncoder[JobFilterDTO] =
    QueryParamEncoder[String].contramap(_.asJson.noSpaces)

  val clientResource: Resource[IO, (FakeAppContainer[IO], Client[IO])] =
    for
      appContainer <- FakeAppContainer[IO]
      httpApi      <- HttpApi[IO](jobService = appContainer.core.services.jobs)
      client        = Client.fromHttpApp(httpApi.routes.orNotFound)
    yield (appContainer, client)

  "CreateJob" - {
    "should return a job with a given id" in {
      clientResource.use { case (appContainer, client) =>
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

          jobs <- client.expect[List[JobDTO]](
            Request[IO](
              method = Method.GET,
              uri = Uri
                .unsafeFromString("/api/jobs")
//                .withQueryParam("filter", JobFilterDTO(remote = false.some))
                .withQueryParam("offset", 0)
                .withQueryParam("limit", 10)
            )
          )

          _ = jobs should contain(awesomeJob)

        } yield succeed
      }
    }
  }
}
