package com.rockthejvm.jobsboard.e2e.journeys

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

import com.rockthejvm.jobsboard.AppContainer
import com.rockthejvm.jobsboard.adapters.in.http.HttpApi
import com.rockthejvm.jobsboard.core.application.ports.in.{
  Command,
  CoreApplication,
  ViewModel
}
import com.rockthejvm.jobsboard.core.application.ports.out.JobRepository
import com.rockthejvm.jobsboard.core.domain.job as Domain
import com.rockthejvm.jobsboard.fixtures.JobFixture
import com.rockthejvm.jobsboard.unit.fakes.FakeAppContainer
import com.rockthejvm.jobsboard.unit.tests.UnitSpec

class JobRoutesSpec extends UnitSpec with JobFixture {
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val clientResource: Resource[IO, (FakeAppContainer[IO], Client[IO])] =
    for
      appContainer <- FakeAppContainer[IO]
      httpApi      <- HttpApi[IO](appContainer.core.app)
      client        = Client.fromHttpApp(httpApi.routes.orNotFound)
    yield (appContainer, client)

  "JobRoutes" - {
    "should return a job with a given id" in {
      clientResource.use { case (appContainer, client) =>
        for {
          jobId <- client.expect[ViewModel.JobId](
            Request[IO](
              method = Method.POST,
              uri = Uri.unsafeFromString("/api/jobs/createJob")
            ).withEntity(createAwesomeJobCommand.asJson)
          ) // TODO: add logging to log the error message
          job   <- client.expect[ViewModel.Job](
            Request[IO](
              method = Method.GET,
              uri = Uri.unsafeFromString(s"/api/jobs/${jobId}")
            )
          )
        } yield job shouldBe awesomeJob
      }
    }
  }
}
