package com.rockthejvm.jobsboard.integration.tests

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.softwaremill.diffx.generic.AutoDerivation
import com.softwaremill.diffx.scalatest.DiffShouldMatcher
import org.scalatest.compatible.Assertion
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class IntegrationSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with DiffShouldMatcher
    with AutoDerivation
