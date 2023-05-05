package com.rockthejvm.jobsboard.adapters.in.config

import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*

final case class AppConfig(
    postgresConfig: PostgresConfig,
    emberConfig: EmberConfig
) derives ConfigReader
