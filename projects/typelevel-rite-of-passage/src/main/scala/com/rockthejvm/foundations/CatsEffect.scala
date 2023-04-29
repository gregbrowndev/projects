package com.rockthejvm.foundations

import scala.io.StdIn
import scala.util.{Random}
import scala.concurrent.duration.*

import cats.effect.{IO, IOApp}
import cats.effect.kernel.Resource
import java.io.{PrintWriter, FileWriter, File}
import cats.MonadError
import cats.effect.kernel.{
  MonadCancel,
  Fiber,
  Spawn,
  GenSpawn,
  Ref,
  Deferred,
  Concurrent,
  Sync,
  Async,
  Temporal
}
import cats.Defer
import scala.concurrent.ExecutionContext

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
  val aFailure: IO[Int] =
    IO.raiseError(new RuntimeException("Something went wrong"))
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
    fib <- IO
      .uncancelable(_ => delayedPrint.onCancel(IO(println("I'm cancelled!"))))
      .start
    _ <- IO.sleep(500.millis) *> IO(println("cancelling fiber")) *> fib.cancel
    _ <- fib.join
  } yield ()

  // resources
  val readingResource = Resource.make(
    IO(
      scala.io.Source.fromFile(
        "src/main/scala/com/rockthejvm/jobsboard/foundations/CatsEffect.scala"
      )
    )
  )(source => IO(println("closing")) *> IO(source.close))
  val readingEffect =
    readingResource.use(source => IO(source.getLines().foreach(println)))

  // composing resources
  val copiedFileResource = Resource.make(
    IO(
      new PrintWriter(
        new FileWriter(new File("src/main/resources/dumpedFile.scala"))
      )
    )
  )(writer => IO(println("closing duplicated file")) *> IO(writer.close()))

  val compositeResource = for {
    source      <- readingResource
    destination <- copiedFileResource
  } yield (source, destination)

  val copyFileEffect = compositeResource.use { case (source, destination) =>
    IO(source.getLines().foreach(destination.println))
  }

  // abstract kinds of computations

  // MonadCancel
  // With MonadCancel we can mark parts of a computation as cancellable and uncancellable
  trait MyMonadCancel[F[_], E] extends MonadError[F, E] {
    // uncancelable relies on a data structure called Poll, but we'll define it more verbosely
    // as the trait below:
    trait CancellationFlagResetter {
      def apply[A](
          fa: F[A]
      ): F[A] // returns the effect with the cancellation flag reset
    }
    def canceled: F[Unit]
    def uncancelable[A](poll: CancellationFlagResetter => F[A]): F[A]
  }

  // monadCancel for IO
  val monadCancelIO: MonadCancel[IO, Throwable] = MonadCancel[IO]
  val uncancellableIO =
    monadCancelIO.uncancelable(_ => IO(42)) // same as IO.uncancelable

  // Spawn - ability to create fibers
  trait MyGenSpawn[F[_], E] extends MonadCancel[F, E] {
    def start[A](fa: F[A]): F[Fiber[F, E, A]] // creates a fiber
    // never, cede, racePair
  }

  trait MySpawn[F[_]] extends GenSpawn[F, Throwable]
  val spawnIO = Spawn[IO]
  val fiber =
    spawnIO.start(delayedPrint) // creates a fiber, same as delayedPrint.start

  // Concurrent - concurrency primatives (atomic references + promises)
  trait MyConcurrent[F[_]] extends Spawn[F] {
    def ref[A](a: A): F[Ref[F, A]]
    def deferred[A]: F[Deferred[F, A]] // for the promise primative
  }
  // with ref and deferred we can express any concurrency primative such as Cyclic Barrier,
  // Count Down Latch, and Semaphore

  // Temporal - ability to suspend computations for a given time
  trait MyTemporal[F[_]] extends Concurrent[F] {
    def sleep(time: FiniteDuration): F[Unit]
  }

  // Sync - ability to suspend synchronous arbitrary expressions in an effect
  trait MySync[F[_]] extends MonadCancel[F, Throwable] with Defer[F] {
    def delay[A](expression: => A): F[A]
    def blocking[A](expression: => A): F[A] // runs on a dedicated thread pool
  }

  // Async - ability to suspend asynchronous computations (i.e. on other thread pools) into an
  // effect managed by CE
  trait MyAsync[F[_]] extends Sync[F] with Temporal[F] {
    def executionContext: F[ExecutionContext]
    def async[A](cb: (Either[Throwable, A] => Unit) => F[Option[F[Unit]]]): F[A]
    // async has the most complex signature you'll likely see, but every type is important
  }

  // In cats-effect we extend IOApp.Simple and override the run method
  // This function returns an IO that will be evaluated internally
  override def run = copyFileEffect
}
