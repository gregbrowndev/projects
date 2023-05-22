package com.rockthejvm.jobsboard.unit.fakes.adapters

import cats.data.EitherT
import cats.effect.*
import cats.implicits.*

import com.rockthejvm.jobsboard.core.application.ports.out.{
  JobRepository,
  TimeAdapter
}
import com.rockthejvm.jobsboard.core.domain.job.*
import com.rockthejvm.jobsboard.fixtures.*

class FakeJobRepository[F[_]: Sync] private (timeAdapter: TimeAdapter[F])
    extends JobRepository[F](timeAdapter) {
  val idSequence: Ref[F, Long]   = Ref.unsafe(0)
  val jobList: Ref[F, List[Job]] = Ref.unsafe(List())

  override def nextIdentity(): F[JobId] = for {
    id <- idSequence.updateAndGet(_ + 1)
    _  <- Sync[F].delay(println(s"[FakeJobRepository] nextIdentity: $id"))
  } yield JobId.fromString(f"00000000-0000-0000-0000-${id}%012d")

  override def create(job: Job): EitherT[F, String, Unit] = for {
    _ <- EitherT.liftF(jobList.update(_ :+ job))
    _ <- EitherT.liftF(
      Sync[F].delay(println("[FakeJobRepository] Created job"))
    )
  } yield ()

  override def all(): F[List[Job]] = for {
    jobs <- jobList.get
  } yield jobs

  override def find(id: JobId): EitherT[F, String, Job] = for {
    jobOpt <- EitherT.right(jobList.get.map(_.find(_.id == id)))
    job    <- EitherT.fromOption(jobOpt, s"Job with ID $id not found")
  } yield job

  override def update(job: Job): EitherT[F, String, Unit] = for {
    jobIndex <- EitherT.right(jobList.get.map(_.indexWhere(_.id == job.id)))
    _        <-
      if jobIndex >= 0 then
        EitherT.liftF(jobList.update(list => list.updated(jobIndex, job)))
      else EitherT.leftT[F, Unit](s"Job with ID ${job.id} not found")
  } yield ()

  override def delete(id: JobId): EitherT[F, String, Unit] = for {
    jobIndex <- EitherT.right(jobList.get.map(_.indexWhere(_.id == id)))
    _        <-
      if jobIndex >= 0 then
        EitherT.liftF(jobList.update(list => list.filterNot(_.id == id)))
      else EitherT.leftT[F, Unit](s"Job with ID $id not found")
  } yield ()
}

object FakeJobRepository {
  def apply[F[_]: Sync](
      timeAdapter: TimeAdapter[F]
  ): Resource[F, FakeJobRepository[F]] =
    Resource.pure(new FakeJobRepository[F](timeAdapter))
}
