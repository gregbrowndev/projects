package com.rockthejvm.foundations

object Cats {

  /*
   type classes
    - Applicative
    - Functor
    - FlatMap
    - Monad
    - ApplicativeError/MonadError
   */

  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list.*

  private val listFunctor  = Functor[List]
  val mappedList: Seq[Int] = listFunctor.map(List(1, 2, 3))(_ + 1)

  // generalizable "mappable" APIs
  def increment[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    functor.map(container)(_ + 1)

  // we can import all the functor functions to expose the map function
  // as an extension method to any container that has an implicit Functor
  // instance in scope
  import cats.syntax.functor.*
  def increment_v2[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    container.map(_ + 1)

  // applicative - pure, wrap existing values into "wrapper" values
  trait MyApplicative[F[_]] extends Functor[F] {
    def pure[A](value: A): F[A]
  }

  import cats.Applicative
  val applicativeList = Applicative[List]
  val aSimpleList     = applicativeList.pure(42)

  // similarly, we can expose extension methods to simplify the code
  import cats.syntax.applicative.*
  val aSimpleList_v2 = 42.pure[List]

  // flatmap
  trait MyFlatMap[F[_]] extends Functor[F] {
    def flatMap[A, B](initialValue: F[A])(f: A => F[B]): F[B]
  }

  import cats.FlatMap
  val flatMapList    = FlatMap[List]
  val flatMappedList = flatMapList.flatMap(List(1, 2, 3))(x => List(x, x + 1))

  // the combination of map from Functor and flatMap gives us for-comprehensions
  import cats.syntax.flatMap.*
  def crossProduct[F[_]: FlatMap, A, B](
      containerA: F[A],
      containerB: F[B]
  ): F[(A, B)] =
    containerA.flatMap(a => containerB.map(b => (a, b)))

  // e.g. we can write the same function as
  def crossProduct_v2[F[_]: FlatMap, A, B](
      containerA: F[A],
      containerB: F[B]
  ): F[(A, B)] =
    for {
      a <- containerA
      b <- containerB
    } yield (a, b)

  // Monad - applicative + flatMap
  trait MyMonad[F[_]] extends Applicative[F] with FlatMap[F] {
    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => pure(f(a)))
  }
  // when we make a type an instance of Monad, we only need to implement the
  // flatMap and pure functions, which gives us map for free

  import cats.Monad
  val monadList = Monad[List]
  // we typically see Monad being used instead of FlatMap on its own
  def crossProduct_v3[F[_]: Monad, A, B](
      containerA: F[A],
      containerB: F[B]
  ): F[(A, B)] =
    for {
      a <- containerA
      b <- containerB
    } yield (a, b)

  // applicative-error - computations that can fail, e.g. Try and Either
  trait MyApplicativeError[F[_]] extends Applicative[F] {
    def raiseError[A, E](error: E): F[A]
  }
  import cats.ApplicativeError
  type ErrorOr[A] = Either[String, A]
  val applicativeEither          = ApplicativeError[ErrorOr, String]
  val desiredValue: ErrorOr[Int] = applicativeEither.pure(42)
  val failedValue: ErrorOr[Int] =
    applicativeEither.raiseError("Something went wrong")
  import cats.syntax.applicativeError.*
  val failedValue_v2: ErrorOr[Int] = "Something went wrong".raiseError

  // monad-error
  trait MyMonadError[F[_], E] extends ApplicativeError[F, E] with Monad[F]
  import cats.MonadError
  val monadErrorEither: MonadError[ErrorOr, String] =
    MonadError[ErrorOr, String]
  // with MonadError we can use flatmap and for-comprehensions with the error value

  def main(args: Array[String]): Unit = {}
}
