package com.rockthejvm.foundations

import java.util.UUID

import cats._
import cats.effect.{IO, IOApp}
import cats.implicits._
import io.circe.generic.auto.*
import io.circe.syntax.*
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.impl.{OptionalValidatingQueryParamDecoderMatcher, QueryParamDecoderMatcher}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import org.http4s.{Header, HttpRoutes}
import org.typelevel.ci.CIString

object Http4s extends IOApp.Simple {

  // simulate an HTTP server with "students" and "courses"
  type Student = String
  case class Instructor(firstName: String, lastName: String)
  case class Course(
      id: UUID,
      title: String,
      year: Int,
      students: List[Student],
      instructorName: String
  )

  object CourseRepository {
    // a "database"
    private val catsEffectCourse = Course(
      UUID.fromString("9d10bd7e-a26d-4781-abe7-7db02a6d843f"),
      "Rock the JVM Ultimate Scala course",
      2022,
      List("Daniel", "Greg"),
      "Martin Odersky"
    )

    private val courses: Map[UUID, Course] = Map(
      catsEffectCourse.id -> catsEffectCourse
    )

    // API
    def findCourseById(courseId: UUID): Option[Course] =
      courses.get(courseId)

    def findCoursesByInstructor(name: String): List[Course] =
      courses.values.filter(_.instructorName == name).toList
  }

  // essential REST endpoints
  // GET localhost:8080/courses?instructor=Martin%20Odersky&year=2022
  // GET localhost:8080/courses/9d10bd7e-a26d-4781-abe7-7db02a6d843f/students

  object InstructorQueryParamMatcher
      extends QueryParamDecoderMatcher[String]("instructor")
  object YearQueryParamMatcher
      extends OptionalValidatingQueryParamDecoderMatcher[Int]("year")

  def courseRoutes[F[_]: Monad]: HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl.*

    HttpRoutes.of[F] {
      case GET -> Root / "courses"
          :? InstructorQueryParamMatcher(instructor)
          +& YearQueryParamMatcher(maybeYear) =>
        val courses = CourseRepository.findCoursesByInstructor(instructor)
        println(s"Received request... Getting courses: $courses")
        maybeYear match {
          case Some(y) =>
            y.fold(
              _ => BadRequest("Parameter 'year' is invalid"),
              year => Ok(courses.filter(_.year == year).asJson)
            )
          case None => Ok(courses.asJson)
        }
      case GET -> Root / "courses" / UUIDVar(courseId) / "students" =>
        CourseRepository.findCourseById(courseId).map(_.students) match {
          case Some(students) =>
            Ok(
              students.asJson,
              Header.Raw(CIString("My-custom-header"), "rockthejvm")
            )
          case None => NotFound(s"No course with $courseId was found")
        }
    }
  }

  def healthEndpoint[F[_]: Monad]: HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl.*
    HttpRoutes.of[F] { case GET -> Root / "health" =>
      Ok("All good")
    }
  }

  // compose multiple routes together
  def allRoutes[F[_]: Monad]: HttpRoutes[F] =
    courseRoutes[F] <+> healthEndpoint[F]

  // alternatively, we can use Router to compose the routes with a path prefix
  def routerWithPathPrefixes = Router(
    "/api"     -> courseRoutes[IO],
    "/private" -> healthEndpoint[IO]
  ).orNotFound

  override def run: IO[Unit] = EmberServerBuilder
    .default[IO]
    // .withHttpApp(allRoutes[IO].orNotFound)
    .withHttpApp(routerWithPathPrefixes)
    .build
    .use(_ => IO.println("Server ready!") *> IO.never)

  /*
  Give it a go:

    $ http GET 'localhost:8080/api/courses?instructor=Martin%20Odersky&year=2022'
    HTTP/1.1 200 OK
    Connection: keep-alive
    Content-Length: 167
    Content-Type: application/json
    Date: Wed, 26 Apr 2023 06:36:25 GMT

    [
        {
            "id": "9d10bd7e-a26d-4781-abe7-7db02a6d843f",
            "instructorName": "Martin Odersky",
            "students": [
                "Daniel",
                "Greg"
            ],
            "title": "Rock the JVM Ultimate Scala course",
            "year": 2022
        }
    ]

    $ http GET 'localhost:8080/api/courses/9d10bd7e-a26d-4781-abe7-7db02a6d843f/students'
    HTTP/1.1 200 OK
    Connection: keep-alive
    Content-Length: 17
    Content-Type: application/json
    Date: Wed, 26 Apr 2023 06:36:17 GMT
    My-custom-header: rockthejvm

    [
        "Daniel",
        "Greg"
    ]

    $ http GET 'localhost:8080/private/health'
    HTTP/1.1 200 OK
    Connection: keep-alive
    Content-Length: 8
    Content-Type: text/plain; charset=UTF-8
    Date: Wed, 26 Apr 2023 06:35:49 GMT

    All good

   */
}
