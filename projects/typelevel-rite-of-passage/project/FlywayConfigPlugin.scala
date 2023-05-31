// import sbt.*
// import Keys.*
// import pureconfig.{ConfigSource, ConfigReader}
// import pureconfig.*
// import pureconfig.generic.derivation.default.*

// final case class FlywayConfig(
//     nThreads: Int,
//     url: String,
//     username: String,
//     password: String
// )

// object FlywayConfig {
//   implicit val configReader: ConfigReader[FlywayConfig] = semiauto.deriveReader[FlywayConfig]
// }

// object FlywayConfigPlugin extends AutoPlugin {
//   override def requires: Plugins = plugins.JvmPlugin

//   object autoImport {
//     lazy val flywayConfig: SettingKey[FlywayConfig] =
//       settingKey[FlywayConfig]("Flyway configuration")
//   }

//   import autoImport.*

//   override def projectSettings: Seq[Def.Setting[?]] = Seq(
//     flywayConfig       := loadFlywayConfig.value,
//     flywayUrl          := flywayConfig.value.url,
//     flywayUser         := flywayConfig.value.user,
//     flywayLocations ++= flywayConfig.value.locations,
//     flywaySchemas      := flywayConfig.value.schemas,
//     flywayPlaceholders := flywayConfig.value.placeholders
//   )

//   private def loadFlywayConfig: Def.Initialize[Task[FlywayConfig]] = Def.task {
//     val baseDirectory = baseDirectory.in(ThisProject).value
//     val configFile    =
//       baseDirectory / "src" / "main" / "resources" / "application.conf"

//     ConfigSource.file(configFile).load[FlywayConfig] match {
//       case Right(config) => config
//       case Left(errors)  =>
//         sys.error(s"Failed to load Flyway configuration: $errors")
//     }
//   }
// }
