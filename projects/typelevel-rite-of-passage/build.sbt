ThisBuild / version                                        := "0.1.0-SNAPSHOT"
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

lazy val rockthejvm    = "com.rockthejvm"
lazy val scala3Version = "3.3.0"

lazy val circeVersion         = "0.14.1"
lazy val catsVersion          = "2.9.0"
lazy val catsEffectVersion    = "3.5.0"
lazy val http4sVersion        = "0.23.19"
lazy val doobieVersion        = "1.0.0-RC1"
lazy val log4catsVersion      = "2.5.0"
lazy val testContainerVersion = "1.19.0"

lazy val server = (project in file("."))
  .enablePlugins(FlywayPlugin, JavaAppPackaging)
  .settings(
    name                 := "typelevel-project",
    scalaVersion         := scala3Version,
    semanticdbEnabled    := true, // for OptimiseImports
    semanticdbVersion    := scalafixSemanticdb.revision,
    semanticdbTargetRoot := baseDirectory.value / ".semanticdb",
    organization         := rockthejvm,
    libraryDependencies ++= Seq(
      "com.github.geirolz"     %% "fly4s-core"                    % "0.0.17",
      "com.github.pureconfig"  %% "pureconfig-core"               % "0.17.2",
      "com.sun.mail"            % "javax.mail"                    % "1.6.2",
      "io.circe"               %% "circe-generic"                 % circeVersion,
      "io.circe"               %% "circe-parser"                  % circeVersion,
      "io.circe"               %% "circe-fs2"                     % circeVersion,
      "io.github.jmcardon"     %% "tsec-http4s"                   % "0.4.0",
      "org.http4s"             %% "http4s-dsl"                    % http4sVersion,
      "org.http4s"             %% "http4s-ember-client"           % http4sVersion,
      "org.http4s"             %% "http4s-ember-server"           % http4sVersion,
      "org.http4s"             %% "http4s-circe"                  % http4sVersion,
      "org.scalacheck"         %% "scalacheck"                    % "1.17.0",          // TODO - should be test only
//      "org.slf4j"               % "slf4j-simple"                  % "2.0.5",
      "org.tpolecat"           %% "doobie-core"                   % doobieVersion,
      "org.tpolecat"           %% "doobie-hikari"                 % doobieVersion,
      "org.tpolecat"           %% "doobie-postgres"               % doobieVersion,
      "org.typelevel"          %% "cats-effect"                   % catsEffectVersion,
      "org.typelevel"          %% "discipline-core"               % "1.5.1",           // TODO - should be test only
      "org.typelevel"          %% "log4cats-slf4j"                % log4catsVersion,
      "org.typelevel"          %% "scalacheck-effect"             % "1.0.4",           // TODO - should be test only
      "org.typelevel"          %% "shapeless3-deriving"           % "3.3.0",
      "ch.qos.logback"          % "logback-classic"               % "1.4.7" % Test,
      "com.softwaremill.diffx" %% "diffx-scalatest-should"        % "0.8.3" % Test,
      "org.typelevel"          %% "cats-laws"                     % catsVersion,       // TODO - should be test only
      "org.typelevel"          %% "cats-effect-laws"              % catsEffectVersion, // TODO - should be test only
      "org.scalactic"          %% "scalactic"                     % "3.2.16",
      "org.scalatest"          %% "scalatest"                     % "3.2.16" % Test,
      "org.typelevel"          %% "cats-effect-testing-scalatest" % "1.4.0" % Test,
      "org.typelevel"          %% "discipline-scalatest"          % "2.2.0" % Test,
      "org.testcontainers"      % "testcontainers"                % testContainerVersion % Test,
      "org.testcontainers"      % "postgresql"                    % testContainerVersion % Test,
      "org.tpolecat"           %% "doobie-scalatest"              % doobieVersion % Test,
      "org.typelevel"          %% "log4cats-noop"                 % log4catsVersion % Test
    ),
    Compile / mainClass  := Some(
      "com.rockthejvm.jobsboard.adapters.in.http.Application"
    ),
    Test / scalacOptions ++= Seq(
      "-Xmax-inlines:100"
    ),
    Global / excludeLintKeys ++= Set(semanticdbTargetRoot)
  )

// Flyway config
// TODO - read these settings from application.conf. See unfinished plugin
flywayUrl       := "jdbc:postgresql://localhost:5433/jobsboard"
flywayUser      := "admin"
flywayPassword  := "admin"
flywayTable     := "migrations"
flywayLocations := Seq("migrations")

// Aliases
addCommandAlias("com", "all compile test:compile")
addCommandAlias("rel", "reload")
addCommandAlias("fix", "all scalafixAll")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")

addCommandAlias("testUnit", "testOnly com.rockthejvm.jobsboard.unit.*")
addCommandAlias("testInt", "testOnly com.rockthejvm.jobsboard.integration.*")
addCommandAlias("testE2e", "testOnly com.rockthejvm.jobsboard.e2e.*")
