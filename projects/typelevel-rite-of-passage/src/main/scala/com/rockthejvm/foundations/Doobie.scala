package com.rockthejvm.foundations

import cats.Functor
import cats.effect.{IOApp, IO, MonadCancelThrow}
import doobie.util.transactor.Transactor
import doobie.implicits.*
import scala.concurrent.ExecutionContext
import doobie.util.ExecutionContexts
import doobie.hikari.HikariTransactor

object Doobie extends IOApp.Simple {
  /*
  - Run the docker compose file to start PostgreSQL
  - Connect to the database:
    docker exec -it typelevel-rite-of-passage-db-1 psql -U docker
  - Run the commands:

    psql> create database typeleveldemo;
    psql> \c typeleveldemo
    psql> create table student(id serial not null, name character varying not null, primary key(id));
    psql> insert into student (id, name) values (1, 'daniel'), (2, 'greg');
   */

  case class Student(id: Int, name: String)

  val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",                          // JDBC connector
    "jdbc:postgresql://localhost:5432/typeleveldemo", // connection string
    "docker",                                         // username
    "docker"                                          // password
  )

  def findAllStudentNames: IO[List[String]] = {
    val query  = sql"select name from student".query[String]
    val action = query.to[List]
    action.transact(xa)
  }

  def saveStudent(student: Student): IO[Int] = {
    val query =
      sql"insert into student (id, name) values (${student.id}, ${student.name})"
    val action = query.update.run
    action.transact(xa)
  }

  def findStudentsByInitial(letter: String): IO[List[Student]] = {
    val selectPart = fr"select id, name"
    val fromPart   = fr"from student"
    val wherePart  = fr"where left (name, 1) = $letter"

    val statement = selectPart ++ fromPart ++ wherePart
    val action    = statement.query[Student].to[List]
    action.transact(xa)
  }

  // how to organise code?
  trait Students[F[_]] { // "repository" using "tagless final" - we can choose effect F later
    def nextIdentity: F[Int]
    def findById(id: Int): F[Option[Student]]
    def findAll: F[List[Student]]
    def save(student: Student): F[Unit]
  }

  object Students {
    def make[F[_]: Functor: MonadCancelThrow](xa: Transactor[F]): Students[F] =
      new Students[F] {
        def nextIdentity: F[Int] =
          sql"select nextval(pg_get_serial_sequence('student','id'))"
            .query[Int]
            .unique
            .transact(xa)

        def findById(id: Int): F[Option[Student]] =
          sql"select id, name from student where id=$id"
            .query[Student]
            .option
            .transact(xa)

        def findAll: F[List[Student]] =
          sql"select id, name from student".query[Student].to[List].transact(xa)

        def save(student: Student): F[Unit] =
          import cats.syntax.functor.*
          sql"insert into student (id, name) values (${student.id}, ${student.name})".update
            .withUniqueGeneratedKeys[Int]("id")
            .transact(xa)
            .map(_ -> ())
      }
  }

  val postgresResource = for {
    ce <- ExecutionContexts.fixedThreadPool[IO](16)
    xa <- HikariTransactor.newHikariTransactor[IO](
      "org.postgresql.Driver",                          // JDBC connector
      "jdbc:postgresql://localhost:5432/typeleveldemo", // connection string
      "docker",                                         // username
      "docker",                                         // password
      ce
    )
  } yield xa

  def smallProgram(studentName: String): IO[Unit] =
    postgresResource.use { xa =>
      val studentRepo = Students.make[IO](xa)
      for {
        id <- studentRepo.nextIdentity
        student = Student(id, studentName)
        _ <- studentRepo.save(student)
        _ <- IO.println(s"The first student of Rock the JVM is ${student.name}")
      } yield ()
    }

  override def run: IO[Unit] = {
    // val newStudent = Student(3, "Bill")
    // saveStudent(newStudent).map(println)

    // findStudentsByInitial("g").map(println)

    smallProgram("Greg") *> smallProgram("John") *> smallProgram("Daniel")
  }
}
