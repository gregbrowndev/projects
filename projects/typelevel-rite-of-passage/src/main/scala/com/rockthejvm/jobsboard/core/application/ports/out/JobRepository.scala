package com.rockthejvm.jobsboard.core.application.ports.out

import cats.data.EitherT
import cats.effect.MonadCancelThrow

import com.rockthejvm.jobsboard.core.application.services.JobFilterDTO
import com.rockthejvm.jobsboard.core.application.services.pagination.PaginationDTO
import com.rockthejvm.jobsboard.core.domain.model.job.{Job, JobId}

trait JobRepository[F[_]: MonadCancelThrow] {
  def nextIdentity(): F[JobId]

  def save(job: Job): EitherT[F, String, Unit]

  def update(job: Job): EitherT[F, String, Unit]

  def delete(id: JobId): EitherT[F, String, Unit]

  def get(id: JobId): EitherT[F, String, Job]

  def all(): F[List[Job]]

  def all(
           filter: JobFilterDTO,
           pagination: PaginationDTO
  ): F[List[Job]]

  // TODO - refactor create/update to save function (collection-oriented API)
}
