package com.rockthejvm.jobsboard.core.application.ports.in

import java.util.UUID

import cats.effect.kernel.Sync

import com.rockthejvm.jobsboard.core.application.ports.in.{Command, ViewModel}
import com.rockthejvm.jobsboard.core.domain.DomainError

trait CoreApplication[F[_]: Sync] {
  // TODO - might be able to use Arrow to abstract over all commands/queries
  
  // commands
  def createJob(cmd: Command.CreateJob): F[Either[String, UUID]]
  def updateJobInfo(
      cmd: Command.UpdateJobInfo
  ): F[Either[DomainError.JobNotFound, Unit]]
  def deleteJob(
      cmd: Command.DeleteJob
  ): F[Either[DomainError.JobNotFound, Unit]]

  // queries
  def findJob(id: UUID): F[Either[DomainError.JobNotFound, ViewModel.Job]]
  def allJobs(): F[List[ViewModel.Job]]
}
