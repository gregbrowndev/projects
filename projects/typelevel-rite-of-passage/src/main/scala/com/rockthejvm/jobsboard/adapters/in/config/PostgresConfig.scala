package com.rockthejvm.jobsboard.adapters.in.config

import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*


final case class PostgresConfig(
    nThreads: Int,
    url: String,
    username: String,
    password: String
) derives ConfigReader
