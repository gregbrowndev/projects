package com.rockthejvm.jobsboard.core.domain.model

// TODO - take a look at https://github.com/Andrea/kantan.csv/blob/715896582bf6faff1700a7aa86f98b3a530244a6/core/shared/src/main/scala/kantan/csv/Errors.scala#L30

//enum DomainError(val message: String) extends Product with Serializable {
//  case JobNotFound(jobId: job.JobId)
//      extends DomainError(s"Job '$jobId' was not found")
//}

object DomainError {
  def jobNotFound(jobId: job.JobId): String = s"Job '$jobId' was not found"
}
