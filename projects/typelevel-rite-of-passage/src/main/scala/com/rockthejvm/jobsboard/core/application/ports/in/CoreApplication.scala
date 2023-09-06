package com.rockthejvm.jobsboard.core.application.ports.in

import java.util.UUID

import cats.effect.kernel.Sync

import com.rockthejvm.jobsboard.core.application.ports.in.{Command, ViewModel}

trait CoreApplication[F[_]: Sync] {
  // TODO - might be able to use Arrow to abstract over all commands/queries

  // commands
  def createJob(cmd: Command.CreateJob): F[Either[String, UUID]]

  def updateJobInfo(
      cmd: Command.UpdateJobInfo
  ): F[Either[String, Unit]]

  def deleteJob(
      cmd: Command.DeleteJob
  ): F[Either[String, Unit]]

  // queries
  def findJob(id: UUID): F[Either[String, ViewModel.Job]]

  def allJobs(): F[List[ViewModel.Job]]
}
