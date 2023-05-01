package com.rockthejvm.jobsboard.http

import cats.implicits.*
import cats.effect.Concurrent
import org.http4s.HttpRoutes

import com.rockthejvm.jobsboard.http.routes.*
import org.http4s.server.Router

class HttpApi[F[_]: Concurrent] private {
  private val healthRoutes = HealthRoutes[F].routes
  private val jobRoutes    = JobRoutes[F].routes

  val endpoints = Router(
    "/api" -> (healthRoutes <+> jobRoutes)
  )
}

object HttpApi {
  def apply[F[_]: Concurrent] = new HttpApi[F]
}
