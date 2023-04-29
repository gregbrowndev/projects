package com.rockthejvm.jobsboard.http

import cats.Monad
import cats.implicits.*
import org.http4s.HttpRoutes

import com.rockthejvm.jobsboard.http.routes.*
import org.http4s.server.Router

class HttpApi[F[_]: Monad] private {
  private val healthRoutes = HealthRoutes[F].routes
  private val jobRoutes    = JobRoutes[F].routes

  val endpoints = Router(
    "/api" -> (healthRoutes <+> jobRoutes)
  )
}

object HttpApi {
  def apply[F[_]: Monad] = new HttpApi[F]
}
