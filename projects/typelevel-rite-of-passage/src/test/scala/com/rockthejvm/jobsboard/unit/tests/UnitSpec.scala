package com.rockthejvm.jobsboard.unit.tests

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.softwaremill.diffx.generic.AutoDerivation
import com.softwaremill.diffx.scalatest.DiffShouldMatcher
import org.scalatest.compatible.Assertion
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

import com.rockthejvm.jobsboard.unit.fakes.FakeAppContainer

abstract class UnitSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with DiffShouldMatcher
    with AutoDerivation {

  def withAppContainer(
      testCode: FakeAppContainer[IO] => IO[Assertion]
  ): IO[Assertion] =
    FakeAppContainer[IO].use(appContainer => testCode(appContainer))
}