package com.rockthejvm.jobsboard.unit.fakes.adapters

import cats.data.EitherT
import cats.effect.*
import cats.implicits.*

import com.rockthejvm.jobsboard.core.application.ports.out.{
  JobRepository,
  TimeAdapter
}
import com.rockthejvm.jobsboard.core.domain.job.*
import com.rockthejvm.jobsboard.core.domain.DomainError as DE
import com.rockthejvm.jobsboard.fixtures.*

class FakeJobRepository[F[_]: Sync] private (timeAdapter: TimeAdapter[F])
    extends JobRepository[F](timeAdapter) {
  val idSequence: Ref[F, Long]   = Ref.unsafe(0)
  val jobList: Ref[F, List[Job]] = Ref.unsafe(List())

  override def nextIdentity(): F[JobId] =
    for id <- idSequence.updateAndGet(_ + 1)
    yield JobId.fromString(f"00000000-0000-0000-0000-${id}%012d")

  override def create(job: Job): EitherT[F, String, Unit] =
    for _ <- EitherT.liftF(jobList.update(_ :+ job))
    yield ()

  override def all(): F[List[Job]] =
    for jobs <- jobList.get
    yield jobs

  override def find(id: JobId): EitherT[F, DE.JobNotFound, Job] =
    for
      jobOpt <- EitherT.right(jobList.get.map(_.find(_.id == id)))
      // TODO - figure out how to remove this explicit type annotation
      job    <- EitherT.fromOption[F](jobOpt, DE.JobNotFound(id)): EitherT[
        F,
        DE.JobNotFound,
        Job
      ]
    yield job

  override def update(job: Job): EitherT[F, DE.JobNotFound, Unit] =
    for jobIndex <- EitherT.right(jobList.get.map(_.indexWhere(_.id == job.id)))
    yield
      if jobIndex >= 0 then
        EitherT.rightT(jobList.update(list => list.updated(jobIndex, job)))
      else EitherT.leftT(DE.JobNotFound(job.id))

  override def delete(id: JobId): EitherT[F, DE.JobNotFound, Unit] =
    for jobIndex <- EitherT.right(jobList.get.map(_.indexWhere(_.id == id)))
    yield
      if jobIndex >= 0 then
        EitherT.rightT(jobList.update(list => list.filterNot(_.id == id)))
      else EitherT.leftT(DE.JobNotFound(id))
}

object FakeJobRepository {
  def apply[F[_]: Sync](
      timeAdapter: TimeAdapter[F]
  ): Resource[F, FakeJobRepository[F]] =
    Resource.pure(new FakeJobRepository[F](timeAdapter))
}
