package com.rockthejvm.jobsboard.core.application.ports.out

import cats.data.EitherT
import cats.effect.MonadCancelThrow
import cats.implicits.*

import com.rockthejvm.jobsboard.core.application.ports.out.TimeAdapter
import com.rockthejvm.jobsboard.core.domain.job.{Job, JobId, JobInfo}
import com.rockthejvm.jobsboard.core.domain.DomainError as DE

trait JobRepository[F[_]: MonadCancelThrow](val timeAdapter: TimeAdapter[F]) {
  // "algebra", i.e. CRUD
  def nextIdentity(): F[JobId]
  def create(job: Job): EitherT[F, String, Unit]
  def all(): F[List[Job]]
  def find(id: JobId): EitherT[F, String, Job]
  def update(job: Job): EitherT[F, String, Unit]
  def delete(id: JobId): EitherT[F, String, Unit]

  // TODO - refactor create/update to save function (collection-oriented API)

  // TODO - Ideally, the factory function should live somewhere else
  def make(ownerEmail: String, jobInfo: JobInfo): F[Job] =
    for {
      id   <- nextIdentity()
      date <- timeAdapter.now()
    } yield Job(
      id = id,
      date = date,
      ownerEmail = ownerEmail,
      active = false,
      jobInfo = jobInfo
    )
}
