package com.rockthejvm.jobsboard.foundations

import scala.io.StdIn
import scala.util.{Random}
import scala.concurrent.duration.*

import cats.effect.{IO, IOApp}
import cats.effect.kernel.Resource
import java.io.PrintWriter
import java.io.FileWriter
import java.io.File

object CatsEffect extends IOApp.Simple {
  /*
    Cats Effect allows us to describe computations as values

    Typeclasses:
    - IO
   * */

  // IO - data structure describing arbitrary computations (including side effects)
  val firstIO: IO[Int] = IO.pure(42)
  val delayedIO: IO[Int] = IO {
    // complex code
    println("I'll tell you the meaning of life")
    42
  }

  def evaluateIO[A](io: IO[A]): Unit = {
    import cats.effect.unsafe.implicits.global // provides implicit runtime
    val meaningOfLife = io.unsafeRunSync()
    println(s"the meaning of life is $meaningOfLife")
  }

  // transformations
  // map + flatMap
  val improvedMeaningOfLife = firstIO.map(_ + 2)
  val printedMeaningOfLife  = firstIO.flatMap(mol => IO(println(mol)))

  // for-comps
  def smallProgram(): IO[Unit] = {
    for {
      line1 <- IO(StdIn.readLine())
      line2 <- IO(StdIn.readLine())
      _     <- IO(println(line1 + line2))
    } yield ()
  }

  // Old-style Scala main function.
//  def main(args: Array[String]): Unit = {
//    evaluateIO(delayedIO)
//  }

  // raise/"catch" errors
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("Something went wrong"))
  val dealWithIt = aFailure.handleErrorWith { case _: RuntimeException =>
    IO(println("I'm still here, no worries"))
  }

  // fibers = "lightweight threads"
  val delayedPrint = IO.sleep(1.second) *> IO(println(Random.nextInt(100)))
  // note: *> operator is flatMap/andThen
  val manyPrints = for {
    _ <- delayedPrint
    _ <- delayedPrint
  } yield ()

  // we can parallelise these fibers
  val manyPrintsPar = for {
    fib1 <- delayedPrint.start
    fib2 <- delayedPrint.start
    _    <- fib1.join
    _    <- fib2.join
  } yield ()

  // cancellation
  val cancelledFiber = for {
    fib <- delayedPrint.onCancel(IO(println("I'm cancelled!"))).start
    _   <- IO.sleep(500.millis) *> IO(println("cancelling fiber")) *> fib.cancel
    _   <- fib.join
    // we should always clean up fibers, even though they are very lightweight they can contain
    // db connections, etc.
  } yield ()

  // uncancellation
  val ignoredCancellation = for {
    fib <- IO.uncancelable(_ => delayedPrint.onCancel(IO(println("I'm cancelled!")))).start
    _   <- IO.sleep(500.millis) *> IO(println("cancelling fiber")) *> fib.cancel
    _   <- fib.join
  } yield ()

  // resources
  val readingResource = Resource.make(
    IO(
      scala.io.Source.fromFile(
        "src/main/scala/com/rockthejvm/jobsboard/foundations/CatsEffect.scala"
      )
    )
  )(source => IO(println("closing")) *> IO(source.close))
  val readingEffect = readingResource.use(source => IO(source.getLines().foreach(println)))

  // composing resources
  val copiedFileResource = Resource.make(
    IO(new PrintWriter(new FileWriter(new File("src/main/resources/dumpedFile.scala"))))
  )(writer => IO(println("closing duplicated file")) *> IO(writer.close()))

  val compositeResource = for {
    source      <- readingResource
    destination <- copiedFileResource
  } yield (source, destination)

  val copyFileEffect = compositeResource.use { case (source, destination) =>
    IO(source.getLines().foreach(destination.println))
  }

  // In cats-effect we extend IOApp.Simple and override the run method
  // This function returns an IO that will be evaluated internally
  override def run = copyFileEffect
}
