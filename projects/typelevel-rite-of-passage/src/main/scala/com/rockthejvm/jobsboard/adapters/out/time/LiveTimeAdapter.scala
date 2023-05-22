package com.rockthejvm.jobsboard.adapters.out.time

import java.time.LocalDateTime

import cats.Applicative
import cats.effect.kernel.Resource
import cats.implicits.*

import com.rockthejvm.jobsboard.core.application.ports.out.TimeAdapter

class LiveTimeAdapter[F[_]: Applicative] extends TimeAdapter[F] {
  override def now(): F[LocalDateTime] = LocalDateTime.now().pure[F]
}

object LiveTimeAdapter {
  def apply[F[_]: Applicative]: Resource[F, LiveTimeAdapter[F]] =
    Resource.pure(new LiveTimeAdapter[F]())
}
