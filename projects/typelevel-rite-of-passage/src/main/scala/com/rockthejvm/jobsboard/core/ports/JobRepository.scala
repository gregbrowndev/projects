package com.rockthejvm.jobsboard.core.ports

import java.util.UUID

import cats.effect.MonadCancelThrow

import com.rockthejvm.jobsboard.core.domain.job.{Job, JobInfo}

trait JobRepository[F[_]: MonadCancelThrow] {
  // "algebra", i.e. CRUD
  def nextIdentity(): F[UUID]
  def create(job: Job): F[Unit]
  def all(): F[List[Job]]
  def find(id: UUID): F[Option[Job]]
  def update(job: Job): F[Unit]
  def delete(id: UUID): F[Unit]

  // TODO - refactor create/update to save function (collection-oriented API)

  // TODO - Ideally, the factory funciton should live somewhere else (otherwise
  // ypu have to implement it multiple times for each adapter)
  def make(ownerEmail: String, jobInfo: JobInfo): F[Job]
}
