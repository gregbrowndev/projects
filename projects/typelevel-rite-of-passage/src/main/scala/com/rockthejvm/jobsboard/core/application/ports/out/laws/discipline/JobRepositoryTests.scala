package com.rockthejvm.jobsboard.core.application.ports.out.laws.discipline

import cats.Functor
import cats.effect.MonadCancelThrow
import cats.effect.implicits.*
import cats.effect.laws.IsEq
import cats.implicits.*
import cats.kernel.Eq
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.scalacheck.effect.PropF
import org.typelevel.discipline.Laws
import com.rockthejvm.jobsboard.core.application.ports.out.JobRepository
import com.rockthejvm.jobsboard.core.application.ports.out.laws.JobRepositoryLaws
import com.rockthejvm.jobsboard.core.application.ports.out.laws.discipline.ArbitraryInstances
import com.rockthejvm.jobsboard.core.domain.job.Job
import org.scalacheck.Prop.forAll

/** Discipline test kit for `JobRepository`.
  */
trait JobRepositoryTests[F[_]: MonadCancelThrow] extends Laws {
  import ArbitraryInstances.*
  import PropF.*

  def laws: JobRepositoryLaws[F]

  // Note: see how cats-effect defines the implicit Eq typeclasses
  // https://github.com/typelevel/cats-effect/blob/series/3.x/laws/shared/src/main/scala/cats/effect/laws/AsyncTests.scala

//  def tests[A](implicit
//      ArbJob: Arbitrary[Job],
//      // CogenJob: Cogen[Job],
//      EqFA: Eq[F[A]],
//      // EqOptionA: Eq[Option[A]],
//      // EqOptionJob: Eq[Option[Job]],
//      // EqJob: Eq[Job],
//      // EqFOptionJob: Eq[F[Option[Job]]]
//      EqOptionJob: Eq[Option[Job]]
//      // FEqOptionJob: F[Eq[Option[Job]]]
//  ): RuleSet = new RuleSet {
//    val name    = "jobRepository"
//    val bases   = Nil
//    val parents = Nil
//    val props   = Seq(
//      LawAdapter.isEqPropF("create and find", laws.saveAndFind)
//    )
//
//    // "create and find"   -> forAll { (job: Job) =>
//    //   repository
//    //     .create(job)
//    //     .flatMap { _ => repository.find(job.id).value }
//    //     .isEqual(repository.find(job.id).value)
//    // },
//    // "create and all"    -> forAll { (job: Job) =>
//    //   repository
//    //     .create(job)
//    //     .flatMap { _ => repository.all() }
//    //     .isEqual(repository.all())
//    // },
//    // "create and delete" -> forAll { (job: Job) =>
//    //   repository
//    //     .create(job)
//    //     .flatMap { _ => repository.delete(job.id) }
//    //     .isEqual(repository.delete(job.id))
//    // },
//    // "find and update"   -> forAll { (job: Job) =>
//    //   repository
//    //     .find(job.id)
//    //     .flatMap { _ => repository.update(job) }
//    //     .isEqual(repository.update(job))
//    // }
//  }

  def tests[A](implicit
     arbJob: Arbitrary[Job],
     eqFJob: Eq[F[Job]],
     eqFEitherTStringUnit: Eq[F[EitherT[F, String, Unit]]],
     eqFEitherTJobNotFoundJob: Eq[F[EitherT[F, DE.JobNotFound, Job]]]
     // CogenJob: Cogen[Job],
//     EqFA: Eq[F[A]],
//     IsEq: IsEq[F[A]],
     // EqOptionA: Eq[Option[A]],
     // EqOptionJob: Eq[Option[Job]],
     // EqJob: Eq[Job],
     // EqFOptionJob: Eq[F[Option[Job]]]
//     EqOptionJob: Eq[Option[Job]]
     // FEqOptionJob: F[Eq[Option[Job]]]
    ) = new SimpleRuleSet(
      name = "JobRepository",
      LawAdapter.isEqPropF("create and find", laws.saveAndFind)
    )
}

object JobRepositoryTests {
  def apply[F[_]: MonadCancelThrow](implicit
      instance: JobRepository[F]
  ): JobRepositoryTests[F] =
    new JobRepositoryTests[F] {
      override val laws: JobRepositoryLaws[F] = JobRepositoryLaws[F]
    }
}

// class LiveJobRepositorySpec extends FunSuite with Discipline with Checkers {
//   implicit def arbJob: Arbitrary[Job] = ???
//   implicit def eqFOptionJob: Eq[F[Option[Job]]] = ???
//   implicit def repo: LiveJobRepository[F] = ???

//   checkAll("LiveJobRepository", JobRepositoryTests[IO](repo).tests)
// }
