// import scalafix.sbt.ScalafixPlugin.autoImport.scalafixDependencies

ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val rockthejvm    = "com.rockthejvm"
lazy val scala3Version = "3.2.1"

lazy val circeVersion               = "0.14.0"
lazy val catsEffectVersion          = "3.3.14"
lazy val http4sVersion              = "0.23.15"
lazy val doobieVersion              = "1.0.0-RC1"
lazy val pureConfigVersion          = "0.17.1"
lazy val log4catsVersion            = "2.4.0"
lazy val tsecVersion                = "0.4.0"
lazy val scalaTestVersion           = "3.2.12"
lazy val scalaTestCatsEffectVersion = "1.4.0"
lazy val testContainerVersion       = "1.17.3"
lazy val logbackVersion             = "1.4.0"
lazy val slf4jVersion               = "2.0.0"
lazy val javaMailVersion            = "1.6.2"
lazy val optimiseImportsVersion     = "0.6.0"

lazy val server = (project in file("."))
  .settings(
    name                := "typelevel-project",
    scalaVersion        := scala3Version,
    semanticdbEnabled   := true, // for OptimseImports
    organization        := rockthejvm,
    libraryDependencies ++= Seq(
      "com.github.pureconfig" %% "pureconfig-core"               % pureConfigVersion,
      "com.sun.mail"           % "javax.mail"                    % javaMailVersion,
      "io.circe"              %% "circe-generic"                 % circeVersion,
      "io.circe"              %% "circe-fs2"                     % circeVersion,
      "io.github.jmcardon"    %% "tsec-http4s"                   % tsecVersion,
      "org.http4s"            %% "http4s-dsl"                    % http4sVersion,
      "org.http4s"            %% "http4s-ember-client"           % http4sVersion,
      "org.http4s"            %% "http4s-ember-server"           % http4sVersion,
      "org.http4s"            %% "http4s-circe"                  % http4sVersion,
      "org.slf4j"              % "slf4j-simple"                  % slf4jVersion,
      "org.tpolecat"          %% "doobie-core"                   % doobieVersion,
      "org.tpolecat"          %% "doobie-hikari"                 % doobieVersion,
      "org.tpolecat"          %% "doobie-postgres"               % doobieVersion,
      "org.typelevel"         %% "cats-effect"                   % catsEffectVersion,
      "org.typelevel"         %% "log4cats-slf4j"                % log4catsVersion,
      "ch.qos.logback"         % "logback-classic"               % logbackVersion             % Test,
      "org.scalatest"         %% "scalatest"                     % scalaTestVersion           % Test,
      "org.tpolecat"          %% "doobie-scalatest"              % doobieVersion              % Test,
      "org.typelevel"         %% "cats-effect-testing-scalatest" % scalaTestCatsEffectVersion % Test,
      "org.testcontainers"     % "testcontainers"                % testContainerVersion       % Test,
      "org.testcontainers"     % "postgresql"                    % testContainerVersion       % Test,
      "org.typelevel"         %% "log4cats-noop"                 % log4catsVersion            % Test
    ),
    Compile / mainClass := Some(
      "com.rockthejvm.jobsboard.adapters.in.http.Application"
    )
  )

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % optimiseImportsVersion

// Aliases
addCommandAlias("com", "all compile test:compile")
addCommandAlias("rel", "reload")
addCommandAlias("fix", "all scalafixAll")
addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
