package com.rockthejvm.jobsboard.adapters.in.http

import cats.effect.Concurrent
import cats.effect.kernel.Resource
import cats.implicits.*
import org.http4s.HttpRoutes
import org.http4s.server.Router
import org.typelevel.log4cats.Logger

import com.rockthejvm.jobsboard.AppContainer
import com.rockthejvm.jobsboard.adapters.in.http.routes.*
import com.rockthejvm.jobsboard.core.ports.in.CoreApplication

class HttpApi[F[_]: Concurrent: Logger] private (
    val app: CoreApplication[F]
) {
  private val healthRoutes = HealthRoutes[F].routes
  private val jobRoutes    = JobRoutes[F](app).routes

  val routes = Router(
    "/api" -> (healthRoutes <+> jobRoutes)
  )
}

object HttpApi {
  def apply[F[_]: Concurrent: Logger](
      app: CoreApplication[F]
  ): Resource[F, HttpApi[F]] = Resource.pure(new HttpApi[F](app))
}
