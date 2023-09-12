package com.rockthejvm.jobsboard.core.application.services

object pagination {
  final case class PaginationDTO(offset: Int, limit: Int)

  object PaginationDTO {
    private val defaultLimit: Int = 10

    def apply(maybeOffset: Option[Int], maybeLimit: Option[Int]): PaginationDTO =
      PaginationDTO(maybeOffset.getOrElse(0), maybeLimit.getOrElse(defaultLimit))

    def default: PaginationDTO = PaginationDTO(0, defaultLimit)
  }
}
