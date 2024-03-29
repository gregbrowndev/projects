package com.rockthejvm.jobsboard.adapters.in.http.validation

import java.net.URL
import scala.util.{Failure, Success, Try}

import cats.*
import cats.data.Validated.*
import cats.data.*
import cats.implicits.*

import com.rockthejvm.jobsboard.core.application.services.*

object validators {

  sealed trait ValidationFailure(val message: String)
  case class EmptyField(fieldName: String)
      extends ValidationFailure(s"$fieldName cannot be empty.")
  case class InvalidUrl(fieldName: String)
      extends ValidationFailure(s"$fieldName is not a valid URL.")

  type ValidationResult[A] = ValidatedNel[ValidationFailure, A]

  trait Validator[A] {
    def validate(value: A): ValidatedNel[ValidationFailure, A]
  }

  private def validateRequired[A](field: A, fieldName: String)(
      required: A => Boolean
  ): ValidationResult[A] =
    if required(field) then field.validNel
    else EmptyField(fieldName).invalidNel

  private def validateUrl(
      field: String,
      fieldName: String
  ): ValidationResult[String] =
    Try(URL(field).toURI) match {
      case Success(_) => field.validNel
      case Failure(_) => InvalidUrl(fieldName).invalidNel
    }

  given createJobCommandValidator: Validator[CreateJobArgsDTO] =
    (cmd: CreateJobArgsDTO) => {
      val CreateJobArgsDTO(
        ownerEmail,
        jobInfo
      ) = cmd

      // TODO: validate email
//    val validOwnerEmail = validateUrl(ownerEmail, "ownerEmail")
      val validJobInfo = jobInfoValidator.validate(jobInfo)

      (
        ownerEmail.validNel,
        validJobInfo
      ).mapN(CreateJobArgsDTO.apply) // ValidatedNel
    }

  given updateJobInfoValidator: Validator[UpdateJobInfoArgsDTO] =
    (cmd: UpdateJobInfoArgsDTO) => {
      val UpdateJobInfoArgsDTO(
        jobId,
        jobInfo
      ) = cmd

      val validJobInfo = jobInfoValidator.validate(jobInfo)

      (
        jobId.validNel,
        validJobInfo
      ).mapN(UpdateJobInfoArgsDTO.apply) // ValidatedNel
    }

  given deleteJobValidator: Validator[DeleteJobArgsDTO] =
    (cmd: DeleteJobArgsDTO) => {
      val DeleteJobArgsDTO(jobId) = cmd

      (
        jobId.validNel
      ).map(DeleteJobArgsDTO.apply) // ValidatedNel
    }

  given jobInfoValidator: Validator[JobInfoDTO] =
    (jobInfo: JobInfoDTO) => {
      val JobInfoDTO(
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
      ) = jobInfo

      val validCompany     = validateRequired(company, "company")(_.nonEmpty)
      val validTitle       = validateRequired(title, "title")(_.nonEmpty)
      val validDescription =
        validateRequired(description, "description")(_.nonEmpty)
      val validOffice      = validateRequired(office, "office")(_.nonEmpty)
      val validExternalUrl = validateUrl(externalUrl, "externalUrl")

      (
        validCompany,
        validTitle,
        validDescription,
        seniority.validNel,
        remote.validNel,
        validOffice,
        country.validNel,
        salaryLo.validNel,
        salaryHi.validNel,
        currency.validNel,
        validExternalUrl,
        image.validNel,
        tags.validNel,
        other.validNel
      ).mapN(JobInfoDTO.apply) // ValidatedNel
    }
}
