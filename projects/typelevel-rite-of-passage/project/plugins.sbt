addSbtPlugin("com.github.sbt"        % "sbt-native-packager"      % "1.9.11")
addSbtPlugin("org.scala-js"          % "sbt-scalajs"              % "1.12.0")
addSbtPlugin("ch.epfl.scala"         % "sbt-scalajs-bundler"      % "0.21.0")
addSbtPlugin("org.portable-scala"    % "sbt-scalajs-crossproject" % "1.2.0")
addSbtPlugin("ch.epfl.scala"         % "sbt-scalafix"             % "0.10.4")
addSbtPlugin("org.scalameta"         % "sbt-scalafmt"             % "2.5.0")
addSbtPlugin("io.github.davidmweber" % "flyway-sbt"               % "5.2.0")

addDependencyTreePlugin

libraryDependencies ++= Seq(
  "com.github.pureconfig" %% "pureconfig-core" % "0.17.2"
)
