package com.rockthejvm.jobsboard.http.routes

import cats.effect.Concurrent
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router

class HealthRoutes[F[_]: Concurrent] extends Http4sDsl[F] {

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
  def apply[F[_]: Concurrent] = new HealthRoutes[F]
}
