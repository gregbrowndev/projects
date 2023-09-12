package com.rockthejvm.jobsboard.unit.fixtures.adapters

import cats.data.EitherT
import cats.effect.*
import cats.implicits.*

import com.rockthejvm.jobsboard.core.application.ports.out.JobRepository
import com.rockthejvm.jobsboard.core.application.services.JobFilterDTO
import com.rockthejvm.jobsboard.core.application.services.pagination.PaginationDTO
import com.rockthejvm.jobsboard.core.domain.model.job.*

class FakeJobRepository[F[_]: Sync] extends JobRepository[F] {
  import com.rockthejvm.jobsboard.core.domain.model.DomainError.*

  val idSequence: Ref[F, Long]   = Ref.unsafe(0)
  val jobList: Ref[F, List[Job]] = Ref.unsafe(List())

  override def nextIdentity(): F[JobId] =
    for id <- idSequence.updateAndGet(_ + 1)
    yield JobId.fromString(f"00000000-0000-0000-0000-${id}%012d")

  override def save(job: Job): EitherT[F, String, Unit] =
    for _ <- EitherT.liftF(jobList.update(_ :+ job))
    yield ()

  override def update(job: Job): EitherT[F, String, Unit] =
    for {
      jobIndex <- EitherT.right(jobList.get.map(_.indexWhere(_.id == job.id)))
      result   <-
        if jobIndex >= 0 then {
          EitherT.liftF(jobList.update(list => list.updated(jobIndex, job)))
        } else {
          EitherT.leftT(jobNotFound(job.id))
        }
    } yield result

  override def delete(id: JobId): EitherT[F, String, Unit] =
    for
      jobIndex <- EitherT.right(jobList.get.map(_.indexWhere(_.id == id)))
      result   <-
        if jobIndex >= 0 then {
          EitherT.liftF(jobList.update(list => list.filterNot(_.id == id)))
        } else {
          EitherT.leftT(jobNotFound(id))
        }
    yield result

  override def get(id: JobId): EitherT[F, String, Job] =
    for
      jobOpt <- EitherT.right(jobList.get.map(_.find(_.id == id)))
      // TODO - figure out how to remove this explicit type annotation
      job    <- EitherT.fromOption[F](jobOpt, jobNotFound(id))
    yield job

  override def all(
  ): F[List[Job]] =
    for jobs <- jobList.get
    yield jobs

  override def all(
      filter: JobFilterDTO,
      pagination: PaginationDTO
  ): F[List[Job]] =
    for jobs <- jobList.get
    yield jobs
}

object FakeJobRepository {
  def apply[F[_]: Sync]: Resource[F, FakeJobRepository[F]] =
    Resource.pure(new FakeJobRepository[F])
}
