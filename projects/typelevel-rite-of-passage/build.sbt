ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val rockthejvm    = "com.rockthejvm"
lazy val scala3Version = "3.2.1"

lazy val circeVersion           = "0.14.0"
lazy val catsVersion            = "2.9.0"
lazy val catsEffectVersion      = "3.3.14"
lazy val http4sVersion          = "0.23.15"
lazy val doobieVersion          = "1.0.0-RC1"
lazy val log4catsVersion        = "2.4.0"
lazy val testContainerVersion   = "1.17.3"
lazy val optimiseImportsVersion = "0.6.0"

lazy val server = (project in file("."))
  .settings(
    name                 := "typelevel-project",
    scalaVersion         := scala3Version,
    semanticdbEnabled    := true, // for OptimseImports
    semanticdbVersion    := scalafixSemanticdb.revision,
    semanticdbTargetRoot := baseDirectory.value / ".semanticdb",
    organization         := rockthejvm,
    libraryDependencies ++= Seq(
      "com.github.pureconfig"  %% "pureconfig-core"               % "0.17.1",
      "com.sun.mail"            % "javax.mail"                    % "1.6.2",
      "io.circe"               %% "circe-generic"                 % circeVersion,
      "io.circe"               %% "circe-fs2"                     % circeVersion,
      "io.github.jmcardon"     %% "tsec-http4s"                   % "0.4.0",
      "org.http4s"             %% "http4s-dsl"                    % http4sVersion,
      "org.http4s"             %% "http4s-ember-client"           % http4sVersion,
      "org.http4s"             %% "http4s-ember-server"           % http4sVersion,
      "org.http4s"             %% "http4s-circe"                  % http4sVersion,
      "org.slf4j"               % "slf4j-simple"                  % "2.0.0",
      "org.tpolecat"           %% "doobie-core"                   % doobieVersion,
      "org.tpolecat"           %% "doobie-hikari"                 % doobieVersion,
      "org.tpolecat"           %% "doobie-postgres"               % doobieVersion,
      "org.typelevel"          %% "cats-effect"                   % catsEffectVersion,
      "org.typelevel"          %% "log4cats-slf4j"                % log4catsVersion,
      "org.typelevel"          %% "cats-laws"                     % catsVersion          % Test,
      "org.typelevel"          %% "cats-effect-laws"              % catsEffectVersion    % Test,
      "ch.qos.logback"          % "logback-classic"               % "1.4.0"              % Test,
      "org.scalatest"          %% "scalatest"                     % "3.2.12"             % Test,
      "com.softwaremill.diffx" %% "diffx-scalatest-should"        % "0.8.3"              % Test,
      "org.tpolecat"           %% "doobie-scalatest"              % doobieVersion        % Test,
      "org.typelevel"          %% "cats-effect-testing-scalatest" % "1.4.0"              % Test,
      "org.testcontainers"      % "testcontainers"                % testContainerVersion % Test,
      "org.testcontainers"      % "postgresql"                    % testContainerVersion % Test,
      "org.typelevel"          %% "log4cats-noop"                 % log4catsVersion      % Test
    ),
    Compile / mainClass  := Some(
      "com.rockthejvm.jobsboard.adapters.in.http.Application"
    ),
    Test / scalacOptions ++= Seq(
      "-Xmax-inlines:100"
    ),
    Global / excludeLintKeys ++= Set(semanticdbTargetRoot)
  )

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % optimiseImportsVersion

// Aliases
addCommandAlias("com", "all compile test:compile")
addCommandAlias("rel", "reload")
addCommandAlias("fix", "all scalafixAll")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
