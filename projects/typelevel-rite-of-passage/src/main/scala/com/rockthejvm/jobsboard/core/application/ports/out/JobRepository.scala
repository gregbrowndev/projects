package com.rockthejvm.jobsboard.core.application.ports.out

import cats.data.EitherT
import cats.effect.MonadCancelThrow

import com.rockthejvm.jobsboard.core.domain.job.{Job, JobId, JobInfo}

trait JobRepository[F[_]: MonadCancelThrow] {
  // "algebra", i.e. CRUD
  def nextIdentity(): F[JobId]
  def create(job: Job): EitherT[F, String, Unit]
  def all(): F[List[Job]]
  def find(id: JobId): EitherT[F, String, Job]
  def update(job: Job): EitherT[F, String, Unit]
  def delete(id: JobId): EitherT[F, String, Unit]

  // TODO - refactor create/update to save function (collection-oriented API)

  // TODO - Ideally, the factory funciton should live somewhere else (otherwise
  // ypu have to implement it multiple times for each adapter)
  def make(ownerEmail: String, jobInfo: JobInfo): F[Job]
}
