package com.rockthejvm.jobsboard.unit.fakes.adapters.tests

import java.time.LocalDateTime
import scala.concurrent.*

import cats.data.EitherT
import cats.effect.*
import cats.effect.implicits.*
import cats.implicits.*
import org.scalatest.compatible.Assertion

import com.rockthejvm.jobsboard.AppContainer
import com.rockthejvm.jobsboard.adapters.in.http.HttpApi
import com.rockthejvm.jobsboard.core.application.ports.in.{
  Command,
  CoreApplication,
  ViewModel
}
import com.rockthejvm.jobsboard.core.application.ports.out.JobRepository
import com.rockthejvm.jobsboard.core.domain.{DomainError, job as Domain}
import com.rockthejvm.jobsboard.fixtures.JobFixture
import com.rockthejvm.jobsboard.unit.fakes.adapters.{
  FakeJobRepository,
  FakeTimeAdapter
}
import com.rockthejvm.jobsboard.unit.tests.UnitSpec
import org.typelevel.discipline.scalatest.Discipline
import com.rockthejvm.jobsboard.core.application.ports.out.laws.discipline.JobRepositoryTests


import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import com.rockthejvm.jobsboard.core.application.ports.out.laws.discipline.ArbitraryInstances
import cats.kernel.Eq
import cats.effect.unsafe.IORuntime
import com.rockthejvm.jobsboard.core.domain.job.Job

class FakeJobRepositoryLawsSpec extends AnyFunSuite with FunSuiteDiscipline with Checkers {
  import ArbitraryInstances.*

  val fakeJobRepoRes: Resource[IO, FakeJobRepository[IO]] =
    for
      timeAdapter <- FakeTimeAdapter[IO]
      jobRepo     <- FakeJobRepository[IO](timeAdapter)
    yield jobRepo
  
  // implicit def eqIO[A: Eq]: Eq[IO[A]] = new Eq[IO[A]] {
  //   def eqv(x: IO[A], y: IO[A]): Boolean = {
  //     val runtime = IORuntime.global

  //     // val resultX = IOApp
  //     //   .Simple(() => runtime.unsafeRunSync(x))
  //     //   .main(Array.empty)
  //     //   .fold(_ => sys.error("Failed to evaluate IO value"), identity)

  //     // val resultY = IOApp
  //     //   .Simple(() => runtime.unsafeRunSync(y))
  //     //   .main(Array.empty)
  //     //   .fold(_ => sys.error("Failed to evaluate IO value"), identity)

  //     Eq[A].eqv(runtime.unsafeRunSync(x), runtime.unsafeRunSync(y))
  //   }
  // }

  implicit def eqIOA[A: Eq]: Eq[IO[A]] = Eq.fromUniversalEquals
  implicit def eqOptionJob: Eq[Option[Job]] = Eq.fromUniversalEquals


  fakeJobRepoRes.use(implicit jobRepo =>
    checkAll("FakeJobRepository", JobRepositoryTests[IO].tests).pure[IO]
  )
}
