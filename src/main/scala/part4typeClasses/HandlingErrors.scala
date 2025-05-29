package part4typeClasses

import cats.{Applicative, Monad}

import scala.concurrent.Future
import scala.util.Try

object HandlingErrors extends App {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    // primitive
    def raiseError[A](e: E): M[A]

    // derived
    def handleErrorWith[A](ma: M[A])(f: E => M[A]): M[A]
    def handleError[A](ma: M[A])(f: E => A): M[A] = handleErrorWith(ma)(e => pure(f(e)))
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A]
  }

  // E can be anything, it's the error type, but it's not has to be a Throwable or a JVM exception
//  trait MyMonadError[M[_], E] extends Monad[M] {
//    def raiseError[A](e: E): M[A] // no longer required because raiseError is a fundamental method of ApplicativeError
//
//  }

  import cats.MonadError
  import cats.instances.either._ // implicit MonadError[Either, E]
  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String] // MonadError[Either[String, *], String]
  val success = monadErrorEither.pure(42) // Either[String, Int] = Right(42)
  val failure = monadErrorEither.raiseError[Int]("Bad things happened") // Either[String, Int] = Left("Bad things happened")

  // recover from an error
  val handledError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "Bad things happened" => 43
    case _ => 0
  } // Either[String, Int] = Right(43)
  val handledError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "Bad things happened" => monadErrorEither.pure(43) // ErrorOr[Int]
    case _ => monadErrorEither.pure(0) // ErrorOr[Int] because ErrorOr is an Either
  } // Either[String, Int] = Right(43)

  // "filtering" an error
  val filteredSuccess = monadErrorEither.ensure(success)("Number too small")(_ > 100) // Either[String, Int] = Left("Number too small")
  // this api is rarely used but is used its extension method

  // try and future
  import cats.instances.try_._ // implicit MonadError[Try, E = Throwable]
  val exception = new RuntimeException("Really bad")
  val pureException = MonadError[Try, Throwable].raiseError(exception) // Try[Nothing] = Failure(exception)

  import cats.instances.future._ // implicit MonadError[Future, Throwable]
  import scala.concurrent.ExecutionContext.Implicits.global
  val errorFuture = MonadError[Future, Throwable].raiseError(exception) // Future[Nothing] = Failure(exception)


  // for the weaker monads (applicative) we have the applicative error => ApplicativeError
  import cats.ApplicativeError
  import cats.data.Validated
  import cats.instances.list._ // implicit Semigroup[List[String]] => ApplicativeError[ErrorsOr, List[String]]
  type ErrorsOr[A] = Validated[List[String], A]
  val applicativeErrorValidated = ApplicativeError[ErrorsOr, List[String]] // ApplicativeError[Validated[List[String], *], List[String]]
  // you have also access to pure, raiseError, handleError, handleErrorWith
  // but not ensure ... why? Because ensure is the fundamental method of MonadError, not ApplicativeError!

  trait MyMonadError[M[_], E] extends Monad[M] {
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A]
  }

  // extension methods
  import cats.syntax.applicative._ // pure
  import cats.syntax.applicativeError._ // raiseError, handleError, handleErrorWith
  import cats.syntax.monadError._ // ensure
  val extendedSuccess = 42.pure[ErrorOr] //requires the implicit ApplicativeError[ErrorsOr, List[String]] .. ErrorOr[Int] = Right(42)
  val extendedError = "Bad things happened".raiseError[ErrorOr, Int] // ErrorOr[Int] = Left(List("Bad things happened"))
  val recoveredError = extendedError.recover {
    case _ => 43
  } // ErrorOr[Int] = Right(43)

  val testedSuccess = success.ensure("Number too small")(_ > 100) // ErrorOr[Int] = Left(List("Number too small"))

  // the actual definition of MonadError is:
  // trait MonadError[M[_], E] extends Monad[M] with ApplicativeError[M, E] {
  // and this is the relation between MonadError and ApplicativeError
}

