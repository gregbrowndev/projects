package com.rockthejvm.jobsboard.adapters.out.db

import java.time.LocalDateTime
import java.util.UUID

import cats.Applicative.*
import cats.data.EitherT
import cats.effect.MonadCancelThrow
import cats.effect.kernel.Resource
import cats.implicits.*
import doobie.implicits.*
import doobie.postgres.*
import doobie.postgres.implicits.*
import doobie.util.Read
import doobie.util.transactor.Transactor

import com.rockthejvm.jobsboard.core.application.ports.out.{
  JobRepository,
  TimeAdapter
}
import com.rockthejvm.jobsboard.core.domain.job.{
  Job,
  JobId,
  JobInfo,
  JobInfoMeta,
  Location,
  Position,
  Salary
}
import com.rockthejvm.jobsboard.core.domain.DomainError as DE

class LiveJobRepository[F[_]: MonadCancelThrow] private (
    xa: Transactor[F],
    timeAdapter: TimeAdapter[F]
) extends JobRepository[F](timeAdapter) {

  override def nextIdentity(): F[JobId] =
    JobId(UUID.randomUUID()).pure[F]

  override def create(job: Job): EitherT[F, String, Unit] =
    val result: F[Unit] = sql"""
      INSERT INTO job (
        id,
        date,
        ownerEmail,
        active,
        company,
        title,
        description,
        seniority,
        remote,
        office,
        country,
        salaryLo,
        salaryHi,
        currency,
        externalUrl,
        image,
        tags,
        other
      ) VALUES (
        ${job.id},
        ${job.date},
        ${job.ownerEmail},
        ${job.active},
        ${job.jobInfo.company},
        ${job.jobInfo.position.title},
        ${job.jobInfo.position.description},
        ${job.jobInfo.position.seniority},
        ${job.jobInfo.position.remote},
        ${job.jobInfo.location.office},
        ${job.jobInfo.location.country},
        ${job.jobInfo.salary.salaryLo},
        ${job.jobInfo.salary.salaryHi},
        ${job.jobInfo.salary.currency},
        ${job.jobInfo.meta.externalUrl},
        ${job.jobInfo.meta.image},
        ${job.jobInfo.meta.tags},
        ${job.jobInfo.meta.other}
      )
    """.update.run
      .transact(xa)
      .map(_ -> ())

    /* No failure possible so pack into RightT (exceptions can occur but let
     * them fail fast, e.g. connection times out). Use EitherT failure for
     * domain/app specific errors like a unique contraint error or concurrency
     * conflict (recoverable) so they can be handled in the business logic */
    EitherT.liftF(result)

  override def all(): F[List[Job]] =
    sql"""
      SELECT
        id,
        date,
        ownerEmail,
        active,
        company,
        title,
        description,
        seniority,
        remote,
        office,
        country,
        salaryLo,
        salaryHi,
        currency,
        externalUrl,
        image,
        tags,
        other
      FROM job
    """
      .query[Job]
      .to[List]
      .transact(xa)

  override def find(id: JobId): EitherT[F, DE.JobNotFound, Job] =
    val result = sql"""
      SELECT
        id,
        date,
        ownerEmail,
        active,
        company,
        title,
        description,
        seniority,
        remote,
        office,
        country,
        salaryLo,
        salaryHi,
        currency,
        externalUrl,
        image,
        tags,
        other
      FROM job
      WHERE id = ${id.value}
    """
      .query[Job]
      .option
      .transact(xa)

    EitherT.fromOptionF(result, DE.JobNotFound(id))

  override def update(job: Job): EitherT[F, DE.JobNotFound, Unit] =
    val result: F[Unit] = sql"""
      UPDATE job SET
        date = ${job.date},
        ownerEmail = ${job.ownerEmail},
        active = ${job.active},
        company = ${job.jobInfo.company},
        title = ${job.jobInfo.position.title},
        description = ${job.jobInfo.position.description},
        seniority = ${job.jobInfo.position.seniority},
        remote = ${job.jobInfo.position.remote},
        office = ${job.jobInfo.location.office},
        country = ${job.jobInfo.location.country},
        salaryLo = ${job.jobInfo.salary.salaryLo},
        salaryHi = ${job.jobInfo.salary.salaryHi},
        currency = ${job.jobInfo.salary.currency},
        externalUrl = ${job.jobInfo.meta.externalUrl},
        image =  ${job.jobInfo.meta.image},
        tags = ${job.jobInfo.meta.tags},
        other =  ${job.jobInfo.meta.other}
      WHERE id = ${job.id}
      """.update.run
      .transact(xa)
      .map(_ -> ())

    EitherT.liftF(result)

  override def delete(id: JobId): EitherT[F, DE.JobNotFound, Unit] =
    // TODO - should return DE.JobNotFound if not found (same as fake)
    val result: F[Unit] = sql"""
      DELETE FROM job
      WHERE id = ${id.value}
      """.update.run
      .transact(xa)
      .map(_ -> ())

    EitherT.liftF(result)
}

object LiveJobRepository {
  given jobRead: Read[Job] = Read[
    (
        JobId,
        LocalDateTime,
        String,  // ownerEmail
        Boolean, // active
        JobInfo
    )
  ]
    .map {
      case (
            id: JobId,
            date: LocalDateTime,
            ownerEmail: String,
            active: Boolean,
            jobInfo: JobInfo
          ) =>
        Job(
          id = id,
          date = date,
          ownerEmail = ownerEmail,
          active = active,
          jobInfo = jobInfo
        )
    }

  given jobIdRead: Read[JobId] = Read[UUID].map { id => JobId(id) }

  given jobInfoRead: Read[JobInfo] = Read[
    (
        String, // company
        Position,
        Location,
        Salary,
        JobInfoMeta
    )
  ]
    .map {
      case (
            company: String,
            position: Position,
            location: Location,
            salary: Salary,
            meta: JobInfoMeta
          ) =>
        JobInfo(
          company = company,
          position = position,
          location = location,
          salary = salary,
          meta = meta
        )
    }

  given positionRead: Read[Position] = Read[
    (
        String,         // title
        String,         // description
        Option[String], // seniority
        Boolean,        // remote
    )
  ]
    .map {
      case (
            title: String,
            description: String,
            seniority: Option[String] @unchecked,
            remote: Boolean,
          ) =>
        Position(
          title = title,
          description = description,
          seniority = seniority,
          remote = remote
        )
    }

  given locationRead: Read[Location] = Read[
    (
        String,         // office
        Option[String], // country
    )
  ]
    .map {
      case (
            office: String,
            country: Option[String] @unchecked,
          ) =>
        Location(
          office = office,
          country = country
        )
    }

  given salaryRead: Read[Salary] = Read[
    (
        Option[Int], // salaryLo
        Option[Int], // salaryHi
        String,      // currency
    )
  ]
    .map {
      case (
            salaryLo: Option[Int] @unchecked,
            salaryHi: Option[Int] @unchecked,
            currency: String
          ) =>
        Salary(salaryLo, salaryHi, currency)
    }

  given metaRead: Read[JobInfoMeta] = Read[
    (
        String,               // externalUrl
        Option[String],       // image
        Option[List[String]], // tags
        Option[String]        // other
    )
  ]
    .map {
      case (
            externalUrl: String,
            image: Option[String] @unchecked,
            tags: Option[List[String]] @unchecked,
            other: Option[String] @unchecked
          ) =>
        JobInfoMeta(
          externalUrl = externalUrl,
          image = image,
          tags = tags,
          other = other
        )
    }

  def apply[F[_]: MonadCancelThrow](
      xa: Transactor[F],
      timeAdapter: TimeAdapter[F]
  ): Resource[F, LiveJobRepository[F]] =
    Resource.pure(new LiveJobRepository[F](xa, timeAdapter))
}
