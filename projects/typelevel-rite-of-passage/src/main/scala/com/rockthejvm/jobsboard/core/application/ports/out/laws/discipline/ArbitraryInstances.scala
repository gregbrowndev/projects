package com.rockthejvm.jobsboard.core.application.ports.out.laws.discipline

import java.time.LocalDateTime
import java.util.UUID

import org.scalacheck.{Arbitrary, Gen}

import com.rockthejvm.jobsboard.core.domain.job.{
  Job,
  JobId,
  JobInfo,
  JobInfoMeta,
  Location,
  Position,
  Salary
}

object ArbitraryInstances {
  import Arbitrary.arbitrary

  // TODO - need to write URL and email generators

  implicit val jobIdArb: Arbitrary[JobId] = Arbitrary {
    for id <- arbitrary[UUID]
    yield JobId(id)
  }

  implicit val jobArb: Arbitrary[Job] = Arbitrary {
    for {
      id         <- arbitrary[JobId]
      date       <- arbitrary[LocalDateTime]
      ownerEmail <- arbitrary[String]
      active     <- arbitrary[Boolean]
      jobInfo    <- arbitrary[JobInfo]
    } yield Job(id, date, ownerEmail, active, jobInfo)
  }

  implicit val jobInfoArb: Arbitrary[JobInfo] = Arbitrary {
    for {
      company  <- arbitrary[String]
      position <- arbitrary[Position]
      location <- arbitrary[Location]
      salary   <- salaryArbForLocation(location)
      meta     <- arbitrary[JobInfoMeta]
    } yield JobInfo(company, position, location, salary, meta)
  }

  implicit val positionArb: Arbitrary[Position] = Arbitrary {
    for {
      title       <- arbitrary[String]
      description <- arbitrary[String]
      seniority   <- Gen.option(
        Gen.oneOf("Junior", "Senior", "Lead", "Principal")
      )
      remote      <- arbitrary[Boolean]
    } yield Position(title, description, seniority, remote)
  }

  implicit val locationArb: Arbitrary[Location] = Arbitrary {
    for {
      office  <- arbitrary[String]
      country <- Gen.option(Gen.oneOf("UK", "US", "NL", "AU", "NZ"))
    } yield Location(office, country)
  }

  def salaryArbForLocation(location: Location): Gen[Salary] =
    location.country match {
      case Some("UK") => salaryArbForCurrency("GBP")
      case Some("US") => salaryArbForCurrency("USD")
      case Some("NL") => salaryArbForCurrency("EUR")
      case Some("AU") => salaryArbForCurrency("AUD")
      case Some("NZ") => salaryArbForCurrency("NZD")
      case _          => salaryArbForCurrency("USD") // Default currency
    }

  def salaryArbForCurrency(currency: String): Gen[Salary] =
    for {
      salaryLo <- Gen.option(Gen.posNum[Int])
      salaryHi <- Gen.option(Gen.posNum[Int])
    } yield Salary(salaryLo, salaryHi, currency)

  implicit val metaArb: Arbitrary[JobInfoMeta] = Arbitrary {
    for {
      externalUrl <- arbitrary[String]
      image       <- Gen.option(arbitrary[String])
      tags        <- Gen.option(Gen.containerOf[List, String](Gen.alphaStr))
      other       <- Gen.option(Gen.alphaStr)
    } yield JobInfoMeta(externalUrl, image, tags, other)
  }
}
