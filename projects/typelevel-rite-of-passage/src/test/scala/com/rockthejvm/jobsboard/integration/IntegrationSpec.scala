package com.rockthejvm.jobsboard.integration

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.softwaremill.diffx.generic.AutoDerivation
import com.softwaremill.diffx.scalatest.DiffShouldMatcher
import org.scalatest.compatible.Assertion
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import com.rockthejvm.jobsboard.adapters.out.db.LiveJobRepository

class IntegrationSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with DiffShouldMatcher
    with AutoDerivation {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  def withLiveJobRepo(
      testCode: LiveJobRepository[IO] => IO[Assertion]
  ): IO[Assertion] =
    Fixture.liveJobRepositoryResource.use(jobRepo => testCode(jobRepo))
}
