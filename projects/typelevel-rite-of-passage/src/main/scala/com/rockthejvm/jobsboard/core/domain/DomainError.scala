package com.rockthejvm.jobsboard.core.domain

enum DomainError(val message: String) extends Product with Serializable {
  case JobNotFound(jobId: job.JobId) extends DomainError(s"Job '$jobId' was not found")
}