package com.rockthejvm.jobsboard.adapters.out.db

import java.time.LocalDateTime
import java.util.UUID

import cats.effect.MonadCancelThrow
import cats.effect.kernel.Resource
import cats.implicits.*
import doobie.implicits.*
import doobie.postgres.*
import doobie.postgres.implicits.*
import doobie.util.Read
import doobie.util.transactor.Transactor

import com.rockthejvm.jobsboard.core.domain.job.{Job, JobInfo, JobInfoMeta, Location, Position, Salary}
import com.rockthejvm.jobsboard.core.ports.JobRepository

class LiveJobRepository[F[_]: MonadCancelThrow] private (xa: Transactor[F])
    extends JobRepository[F] {

  override def nextIdentity(): F[UUID] =
    UUID.randomUUID().pure[F]

  override def make(ownerEmail: String, jobInfo: JobInfo): F[Job] =
    for {
      id <- nextIdentity()
      date = LocalDateTime.now()
    } yield Job(
      id = id,
      date = date,
      ownerEmail = ownerEmail,
      active = false,
      jobInfo = jobInfo
    )

  override def create(job: Job): F[Unit] =
    sql"""
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
        ${job.jobInfo.salary.flatMap(s => s.salaryLo)},
        ${job.jobInfo.salary.flatMap(s => s.salaryHi.pure)},
        ${job.jobInfo.salary.flatMap(s => s.currency.pure)},
        ${job.jobInfo.meta.externalUrl},
        ${job.jobInfo.meta.image},
        ${job.jobInfo.meta.tags},
        ${job.jobInfo.meta.other}
      )
    """.update.run
      .transact(xa)
      .map(_ -> ())

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

  override def find(id: UUID): F[Option[Job]] =
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
      WHERE id = $id
    """
      .query[Job]
      .option
      .transact(xa)

  override def update(job: Job): F[Unit] =
    sql"""
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
        salaryLo = ${job.jobInfo.salary.flatMap(s => s.salaryLo)},
        salaryHi = ${job.jobInfo.salary.flatMap(s => s.salaryHi.pure)},
        currency = ${job.jobInfo.salary.flatMap(s => s.currency.pure)},
        externalUrl = ${job.jobInfo.meta.externalUrl},
        image =  ${job.jobInfo.meta.image},
        tags = ${job.jobInfo.meta.tags},
        other =  ${job.jobInfo.meta.other}
      WHERE id = ${job.id}
    """.update.run
      .transact(xa)
      .map(_ -> ())

  override def delete(id: UUID): F[Unit] =
    sql"""
      DELETE FROM job
      WHERE id = $id
    """.update.run
      .transact(xa)
      .map(_ -> ())
}

object LiveJobRepository {
  given jobRead: Read[Job] = Read[
    (
        UUID,          // id
        LocalDateTime, // date
        String,        // ownerEmail
        Boolean,       // active
        JobInfo
    )
  ]
    .map {
      case (
            id: UUID,
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

  given jobInfoRead: Read[JobInfo] = Read[
    (
        String, // company
        Position,
        Location,
        Option[Salary],
        JobInfoMeta
    )
  ]
    .map {
      case (
            company: String,
            position: Position,
            location: Location,
            salary: Option[Salary] @unchecked,
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
        Int,         // salaryHi
        String,      // currency
    )
  ]
    .map {
      case (
            salaryLo: Option[Int] @unchecked,
            salaryHi: Int,
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
      xa: Transactor[F]
  ): Resource[F, LiveJobRepository[F]] =
    Resource.pure(new LiveJobRepository[F](xa))
}
