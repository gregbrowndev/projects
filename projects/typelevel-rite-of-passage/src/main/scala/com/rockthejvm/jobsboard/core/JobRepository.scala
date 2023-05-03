package com.rockthejvm.jobsboard.core

import java.util.UUID

import cats.effect.MonadCancelThrow

import com.rockthejvm.jobsboard.domain.job.Job

trait JobRepository[F[_]: MonadCancelThrow] {
  // "algebra", i.e. CRUD
  def nextIdentity(): F[UUID]
  def create(job: Job): F[Unit]
  def all(): F[List[Job]]
  def find(id: UUID): F[Option[Job]]
  def update(job: Job): F[Unit]
  def delete(id: UUID): F[Unit]
}
