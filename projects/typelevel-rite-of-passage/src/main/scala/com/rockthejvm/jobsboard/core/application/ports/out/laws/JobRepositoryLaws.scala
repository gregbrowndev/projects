package com.rockthejvm.jobsboard.core.application.ports.out.laws

import cats.Applicative.*
import cats.data.EitherT
import cats.effect.implicits.*
import cats.effect.kernel.MonadCancelThrow
import cats.laws.{IsEq, IsEqArrow}
import cats.implicits.*

import com.rockthejvm.jobsboard.core.application.ports.out.JobRepository
import com.rockthejvm.jobsboard.core.domain.DomainError
import com.rockthejvm.jobsboard.core.domain.job.Job
import cats.Applicative
import cats.kernel.Eq

// class DetailedEq[F[_]: Applicative, A](implicit eqA: Eq[A]) extends Eq[F[A]] {
//   def eqv(x: F[A], y: F[A]): Boolean =
//     // x.map(Some(_)) === y.map(Some(_))
//     for
//       xval <- x
//       yval <- y
//     yield xval === yval

//   override def neqv(x: F[A], y: F[A]): Boolean =
//     !eqv(x, y)

//   override def toString: String =
//     s"DetailedEq[${eqA}]"
// }

/* TODOs
 *
 * - Abstract out Job for any kind of Repository.
 *
 * Will need to introduce Aggregate typeclass which exposes the ID which can be
 * passed to the repository */

/** Laws that must be obeyed by any `JobRepository`.
  */
trait JobRepositoryLaws[F[_]: MonadCancelThrow] {
  def repo: JobRepository[F]

  def saveAndFind(job: Job): IsEq[F[Option[Job]]] =
    // TODO - could simplify code to convert all to OptionT,
    //  e.g. repo.create(job).toOption
    type ResultError = String | DomainError.JobNotFound
    val resultF =
      for
        _      <- repo.create(job)
        result <- repo.find(job.id).leftWiden[ResultError]
        _      <- EitherT.liftF(
          println(s"[JobRepositoryLaws] result:  $result").pure[F]
        )
      yield result

    resultF.toOption.value <-> Some(job).pure[F]
    // for
    //   result   <- resultF.toOption.value
    //   expected <- Some(job).pure[F]
    // yield result <-> expected

}

object JobRepositoryLaws {
  def apply[F[_]: MonadCancelThrow](implicit
      instance: JobRepository[F]
  ): JobRepositoryLaws[F] = new JobRepositoryLaws[F] { override val repo: JobRepository[F] = instance }
}
