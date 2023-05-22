package com.rockthejvm.jobsboard.unit

import cats.effect.*
import cats.effect.implicits.*
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
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

abstract class UnitSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with Http4sDsl[IO]

class JobRoutesSpec extends UnitSpec with JobFixture {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val clientResource: Resource[IO, (FakeAppContainer[IO], HttpApp[IO])] =
    for
      appContainer <- FakeAppContainer[IO]
      httpApi      <- HttpApi[IO](appContainer.core.app)
      // client       <- Client.fromHttpApp(httpApi.routes.orNotFound)
      client        = httpApi.routes.orNotFound
    yield (appContainer, client)

  "JobRoutes" - {
    "should return a job with a given id" in {
      clientResource.use { case (appContainer, client) =>
        for {
          _         <- IO(println("Creating job"))
          _         <- client.run(
            Request(
              method = Method.POST,
              uri = Uri.unsafeFromString("/api/jobs")
            ).withEntity(awesomeJob.asJson)
          )
          _         <- IO(println("Created job"))
          response  <- client.run(
            Request(
              method = Method.GET,
              uri = Uri.unsafeFromString(s"/api/jobs/${awesomeJobId}")
            )
          )
          _         <- IO(println("Found job")) *> IO(println(response.body))
          _         <- IO(println("JobRepository contains: ")) *> IO(
            appContainer.adapters.jobRepo.all().map(_.count)
          )
          retrieved <- response.as[ViewModel.Job]
          _         <- IO(println("Decoded response")) *> IO(println(retrieved))
        } yield {
          response.status shouldBe Status.Ok
          retrieved shouldBe awesomeJob
        }
      }
    }
  }
}
