package com.rockthejvm.jobsboard.e2e

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{IO, Resource}
import com.softwaremill.diffx.generic.AutoDerivation
import com.softwaremill.diffx.scalatest.DiffShouldMatcher
import org.http4s.client.Client
import org.scalatest.compatible.Assertion
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import com.rockthejvm.jobsboard.adapters.in.http.HttpApi
import com.rockthejvm.jobsboard.unit.fixtures.FakeAppContainer

abstract class E2eSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with DiffShouldMatcher
    with AutoDerivation {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  def withClient(
      testCode: Client[IO] => IO[Assertion]
  ): IO[Assertion] =
    // TODO: use real database and server
    val clientResource: Resource[IO, Client[IO]] =
      for
        appContainer <- FakeAppContainer[IO]
        httpApi      <- HttpApi[IO](jobService = appContainer.core.services.jobs)
        client        = Client.fromHttpApp(httpApi.routes.orNotFound)
      yield client

    clientResource.use(client => testCode(client))
}
