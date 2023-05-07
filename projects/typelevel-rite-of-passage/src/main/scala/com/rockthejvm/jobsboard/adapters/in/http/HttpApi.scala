package com.rockthejvm.jobsboard.adapters.in.http

import cats.effect.Concurrent
import cats.effect.kernel.Resource
import cats.implicits.*
import org.http4s.HttpRoutes
import org.http4s.server.Router
import org.typelevel.log4cats.Logger

import com.rockthejvm.jobsboard.AppContainer
import com.rockthejvm.jobsboard.adapters.in.http.routes.*

class HttpApi[F[_]: Concurrent: Logger] private (
    appContainer: AppContainer[F]
) {
  private val healthRoutes = HealthRoutes[F].routes
  private val jobRoutes    = JobRoutes[F](appContainer.dbContainer.jobRepo).routes

  val routes = Router(
    "/api" -> (healthRoutes <+> jobRoutes)
  )
}

object HttpApi {
  def apply[F[_]: Concurrent: Logger](
      appContainer: AppContainer[F]
  ): Resource[F, HttpApi[F]] = Resource.pure(new HttpApi[F](appContainer))
}
