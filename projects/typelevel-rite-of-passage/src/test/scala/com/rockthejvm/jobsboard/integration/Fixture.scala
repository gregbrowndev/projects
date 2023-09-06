package com.rockthejvm.jobsboard.integration

import cats.data.EitherT
import cats.effect.IO
import cats.effect.implicits.*
import cats.effect.kernel.Resource
import cats.implicits.*
import doobie.hikari.HikariTransactor
import fly4s.core.Fly4s
import fly4s.core.data.{Fly4sConfig, Location, ValidatePattern}
import fly4s.implicits.*
import org.flywaydb.core.api.output.MigrateResult
import org.testcontainers.containers.PostgreSQLContainer

import com.rockthejvm.jobsboard.adapters.in.config.PostgresConfig
import com.rockthejvm.jobsboard.adapters.out.db.{
  LiveJobRepository,
  TransactorFactory
}
import com.rockthejvm.jobsboard.adapters.out.time.LiveTimeAdapter

object Fixture {

  /** Creates a `Resource` to provide a Fly4s client that connects to the
    * database using `config`. The Fly4s client provides an API to manage the
    * database schema.
    */
  def fly4sRes(config: PostgresConfig): Resource[IO, Fly4s[IO]] =
    Fly4s.make[IO](
      url = config.url,
      user = Some(config.username),
      password = Some(config.password.toArray[Char]),
      config = Fly4sConfig(
        table = "migrations",
        locations = List(Location("migrations")),
        ignoreMigrationPatterns = List(ValidatePattern.ignorePendingMigrations)
      )
    )

  def migrateDb(fly4s: Fly4s[IO]): IO[MigrateResult] =
    fly4s.validateAndMigrate.result

  def getPostgresConfig(
      postgres: PostgreSQLContainer[Nothing]
  ): PostgresConfig =
    PostgresConfig(
      nThreads = 1,
      url = postgres.getJdbcUrl,
      username = postgres.getUsername,
      password = postgres.getPassword
    )

  val postgresResource: Resource[IO, PostgreSQLContainer[Nothing]] =
    val acquire: IO[PostgreSQLContainer[Nothing]] = IO {
      val container = new PostgreSQLContainer("postgres")
      println("[TEST] starting postgres container")
      container.start()
      println("[TEST] started postgres container")
      container
    }

    val acquireAndMigrate: IO[PostgreSQLContainer[Nothing]] =
      for
        container <- acquire
        result    <- {
          // Migrate the database
          val config        = getPostgresConfig(container)
          val fly4sResource = fly4sRes(config)

          println("[TEST] starting Fly4s migrations")
          fly4sResource.use(fly4s =>
            for migrationResult <- migrateDb(fly4s)
            yield {
              println("[TEST] migrated database complete...")
              if migrationResult.success then {
                println("[TEST] Successfully migrated database")
                container
              } else {
                println("[TEST] Failed to migrate database")
                // IO.raiseError(new Exception("Migration failed"))
                throw new Exception("Migration failed")
              }
            }
          )
        }
      yield result

    val release: PostgreSQLContainer[Nothing] => IO[Unit] =
      container => IO(container.stop())

    Resource.make(acquireAndMigrate)(release)

  val transactorResource: Resource[IO, HikariTransactor[IO]] =
    for
      postgres <- postgresResource
      config    = getPostgresConfig(postgres)
      xa       <- TransactorFactory[IO](config)
    yield xa

  val liveJobRepositoryResource: Resource[IO, LiveJobRepository[IO]] =
    for
      xa          <- Fixture.transactorResource
      timeAdapter <- LiveTimeAdapter[IO]
      jobRepo     <- LiveJobRepository[IO](xa, timeAdapter)
    yield jobRepo
}
