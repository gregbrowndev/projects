package com.rockthejvm.jobsboard.unit.fixtures.adapters

import java.time.LocalDateTime

import cats.Applicative
import cats.effect.kernel.Resource
import cats.implicits.*

import com.rockthejvm.jobsboard.core.application.ports.out.TimeAdapter

class FakeTimeAdapter[F[_]: Applicative] extends TimeAdapter[F] {
  private var timeNow = LocalDateTime.parse("2023-01-01T00:00:00")

  def now(): F[LocalDateTime] = timeNow.pure[F]

  // Tester API
  def setTime(dateTime: LocalDateTime): F[Unit] =
    timeNow = dateTime
    ().pure[F]
}

object FakeTimeAdapter {
  def apply[F[_]: Applicative]: Resource[F, FakeTimeAdapter[F]] =
    Resource.pure(new FakeTimeAdapter[F]())
}
