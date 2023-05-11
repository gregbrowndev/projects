package com.rockthejvm.jobsboard.core.application.ports.in

import java.util.UUID

object Command {
  sealed trait Command
  case class CreateJob(ownerEmail: String, jobInfo: ViewModel.JobInfo)
      extends Command
  case class UpdateJobInfo(jobId: UUID, jobInfo: ViewModel.JobInfo)
      extends Command
  case class DeleteJob(jobId: UUID) extends Command
}
