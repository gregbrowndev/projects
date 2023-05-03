package com.rockthejvm.jobsboard.adapters.in.http.routes

import cats.effect.Concurrent
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import cats.MonadThrow
import org.typelevel.log4cats.Logger

class HealthRoutes[F[_]: Concurrent: Logger] extends Http4sDsl[F] {

  // GET /health
  private val healthRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root =>
      Ok("All going great!")
  }

  val routes = Router(
    "/health" -> healthRoutes
  )
}

object HealthRoutes {
  def apply[F[_]: Concurrent: Logger] = new HealthRoutes[F]
}
