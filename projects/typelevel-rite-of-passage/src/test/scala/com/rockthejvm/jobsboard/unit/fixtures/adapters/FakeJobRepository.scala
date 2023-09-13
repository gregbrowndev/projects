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
    yield filterJobs(jobs, filter, pagination)

  private def filterJobs(
      jobs: List[Job],
      filter: JobFilterDTO,
      pagination: PaginationDTO
  ): List[Job] = {
    val filteredJobs = jobs.filter { job =>
      (filter.companies.isEmpty || filter.companies.forall(
        _.contains(job.jobInfo.company)
      )) &&
      (filter.locations.isEmpty || filter.locations.forall(
        _.contains(job.jobInfo.location)
      )) &&
      (filter.countries.isEmpty || filter.countries.exists(
        _.contains(job.jobInfo.location.country)
      )) &&
      (filter.seniorities.isEmpty || filter.seniorities.forall(seniorities =>
        job.jobInfo.position.seniority.exists(s => seniorities.contains(s))
      )) &&
      (filter.tags.isEmpty || filter.tags.forall(tags =>
        job.jobInfo.meta.tags.exists(jobTags =>
          tags.forall(jobTag => jobTags.contains(jobTag))
        )
      )) &&
      (filter.maxSalary.isEmpty || filter.maxSalary.forall(maxSalary =>
        job.jobInfo.salary.salaryHi.exists(s => s <= maxSalary)
      )) &&
      (filter.remote.isEmpty || filter.remote.forall(remote =>
        remote == job.jobInfo.position.remote
      ))
    }

    filteredJobs.slice(pagination.offset, pagination.offset + pagination.limit)
  }
}

object FakeJobRepository {
  def apply[F[_]: Sync]: Resource[F, FakeJobRepository[F]] =
    Resource.pure(new FakeJobRepository[F])
}
