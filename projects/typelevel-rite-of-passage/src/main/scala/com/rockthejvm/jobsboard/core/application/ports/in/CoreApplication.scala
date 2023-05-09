package com.rockthejvm.jobsboard.core.application.ports.in

import cats.effect.kernel.Async

import com.rockthejvm.jobsboard.core.domain.job.{Job, JobId, JobInfo}

trait CoreApplication[F[_]: Async] {
  def allJobs(): F[List[Job]]
  def createJob(jobInfo: JobInfo): F[Either[String, JobId]]
  def findJob(id: JobId): F[Either[String, Job]]
  def updateJob(id: JobId, jobInfo: JobInfo): F[Either[String, Unit]]
  def deleteJob(id: JobId): F[Either[String, Unit]]
}
