package com.rockthejvm.jobsboard.unit.fakes.adapters

import cats.data.EitherT
import cats.effect.*
import cats.implicits.*

import com.rockthejvm.jobsboard.core.application.ports.out.JobRepository
import com.rockthejvm.jobsboard.core.domain.job.*
import com.rockthejvm.jobsboard.fixtures.*

class FakeJobRepository[F[_]: Sync] private extends JobRepository[F] {
  val idSequenceRef: F[Ref[F, Long]]   = Ref.of(1)
  val jobListRef: F[Ref[F, List[Job]]] = Ref.of(List())

  override def nextIdentity(): F[JobId] = for {
    idSequence <- idSequenceRef
    id         <- idSequence.modify(seq => (seq + 1, seq))
  } yield JobId.fromString(f"00000000-0000-0000-0000-${id}%012d")

  override def create(job: Job): EitherT[F, String, Unit] = for {
    jobList <- EitherT.liftF(jobListRef)
    _       <- EitherT.liftF(jobList.update(_ :+ job))
  } yield ()

  override def all(): F[List[Job]] = for {
    jobList <- jobListRef
    jobs    <- jobList.get
  } yield jobs

  override def find(id: JobId): EitherT[F, String, Job] = for {
    jobList <- EitherT.liftF(jobListRef)
    jobOpt  <- EitherT.right(jobList.get.map(_.find(_.id == id)))
    job     <- EitherT.fromOption(jobOpt, s"Job with ID $id not found")
  } yield job

  override def update(job: Job): EitherT[F, String, Unit] = for {
    jobList  <- EitherT.liftF(jobListRef)
    jobIndex <- EitherT.right(jobList.get.map(_.indexWhere(_.id == job.id)))
    _        <-
      if jobIndex >= 0 then
        EitherT.liftF(jobList.update(list => list.updated(jobIndex, job)))
      else EitherT.leftT[F, Unit](s"Job with ID ${job.id} not found")
  } yield ()

  override def delete(id: JobId): EitherT[F, String, Unit] = for {
    jobList  <- EitherT.liftF(jobListRef)
    jobIndex <- EitherT.right(jobList.get.map(_.indexWhere(_.id == id)))
    _        <-
      if jobIndex >= 0 then
        EitherT.liftF(jobList.update(list => list.filterNot(_.id == id)))
      else EitherT.leftT[F, Unit](s"Job with ID $id not found")
  } yield ()
}

object FakeJobRepository {
  def apply[F[_]: Sync]: Resource[F, FakeJobRepository[F]] =
    Resource.pure(new FakeJobRepository[F]())
}
