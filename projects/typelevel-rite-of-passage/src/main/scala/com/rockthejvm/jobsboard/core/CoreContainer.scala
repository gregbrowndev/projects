package com.rockthejvm.jobsboard.core

import cats.effect.kernel.{Async, Resource}

import com.rockthejvm.jobsboard.core.ports.{JobRepository}


trait DBContainer[F[_]: Async] {
  val jobRepo: JobRepository[F]
}

object CoreContainer {
  // This is where command/event handlers (application services) and perhaps
  // domain services can be instantiated and stored in a CoreContainer case 
  // class
}
