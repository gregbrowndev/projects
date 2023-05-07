package com.rockthejvm.jobsboard.adapters.in.config

import com.comcast.ip4s.{Host, Port}
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert
import pureconfig.generic.derivation.default.*

// we're using something from pureconfig to automatically
// derive the ConfigReader typeclass, i.e.
//   given configReader: ConfigReader[EmberConfig]
final case class EmberConfig(host: Host, port: Port) derives ConfigReader

object EmberConfig {
  // we need given ConfigReader[Host] + given ConfigReader[Port]
  //  => compiler generates ConfigReader[EmberConfig]
  given hostReader: ConfigReader[Host] =
    ConfigReader[String].emap { // emap = either-map
      hostString =>
        Host.fromString(hostString) match {
          case None       => // error, return a Left
            Left(
              CannotConvert(
                hostString,
                Host.getClass.toString,
                s"Invalid host string: $hostString"
              )
            )
          case Some(host) => Right(host)
        }
    }

  given portReader: ConfigReader[Port] =
    ConfigReader[Int].emap { // emap = either-map
      portInt =>
        Port
          .fromInt(portInt)
          .toRight( // note: this is equiv. to above
            CannotConvert(
              portInt.toString,
              Port.getClass.toString,
              s"Invalid port number: $portInt"
            )
          )
    }
}
