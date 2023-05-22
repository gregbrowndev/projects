package com.rockthejvm.jobsboard.core.application.ports.out

import java.time.LocalDateTime

import cats.Applicative

trait TimeAdapter[F[_]: Applicative] {
  def now(): F[LocalDateTime]
}
