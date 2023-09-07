package com.rockthejvm.jobsboard.adapters.in.http.validation

import cats.*
import cats.implicits.*
import cats.data.*
import cats.data.Validated.*

import java.net.URL
import scala.util.{Failure, Success, Try}
import com.rockthejvm.jobsboard.core.application.ports.in.{Command, ViewModel}

object validators {

  sealed trait ValidationFailure(val message: String)
  case class EmptyField(fieldName: String) extends ValidationFailure(s"$fieldName cannot be empty.")
  case class InvalidUrl(fieldName: String) extends ValidationFailure(s"$fieldName is not a valid URL.")


  type ValidationResult[A] = ValidatedNel[ValidationFailure, A]

  trait Validator[A] {
    def validate(value: A): ValidatedNel[ValidationFailure, A]
  }

  def validateRequired[A](field: A, fieldName: String)(required: A => Boolean): ValidationResult[A] =
    if (required(field)) field.validNel
    else EmptyField(fieldName).invalidNel

  def validateUrl(field: String, fieldName: String): ValidationResult[String] =
    Try(URL(field).toURI) match {
      case Success(_) => field.validNel
      case Failure(_) => InvalidUrl(fieldName).invalidNel
    }

  given createJobCommandValidator: Validator[Command.CreateJob] = (cmd: Command.CreateJob) => {
    val Command.CreateJob(
          ownerEmail,
          jobInfo
        ) = cmd

    val validOwnerEmail = validateUrl(ownerEmail, "ownerEmail")
    val validJobInfo = jobInfoValidator.validate(jobInfo)

    (
      validOwnerEmail,
      validJobInfo
    ).mapN(Command.CreateJob.apply)  // ValidatedNel
  }


  given jobInfoValidator: Validator[ViewModel.JobInfo] = (jobInfo: ViewModel.JobInfo) => {
    val ViewModel.JobInfo(
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

    val validCompany = validateRequired(company, "company")(_.nonEmpty)
    val validTitle = validateRequired(title, "title")(_.nonEmpty)
    val validDescription = validateRequired(description, "description")(_.nonEmpty)
    val validOffice = validateRequired(office, "office")(_.nonEmpty)
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
    ).mapN(ViewModel.JobInfo.apply)  // ValidatedNel
  }
}
