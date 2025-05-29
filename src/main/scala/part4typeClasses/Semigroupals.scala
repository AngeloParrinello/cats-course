package part4typeClasses

import scala.concurrent.Future

object Semigroupals extends App {

  trait MySemigroupal[F[_]] {
    // combine two contexts
    // you have two generics value (fa and fb) and you want to combine them into a single context F[(A, B)]
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }


  import cats.Semigroupal
  import cats.instances.option._ // Semigroupal[Option]
  val optionSemigroupal: Semigroupal[Option] = Semigroupal[Option]
  val aTupledOption: Option[(Int, String)] = optionSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string"))
  val aNoneTupled: Option[(Int, String)] = optionSemigroupal.product(Some(123), None) // None

  import cats.instances.future._ // Semigroupal[Future]
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
  val aTupledFuture: Future[(Int, String)] = Semigroupal[Future].product(Future(123), Future("a future string")) // Future((123, "a future string"))


  import cats.instances.list._ // Semigroupal[List]
  val aTupledList = Semigroupal[List].product(List(1, 2), List("a", "b")) // List((1, "a"), (1, "b"), (2, "a"), (2, "b")), this is the cartesian product!! we are using the monad class of logic here
  println(aTupledList)

  // TODO: implement product with monads to demonstrate that Semigroupal uses monadic logic
  // you should use flatmap, pure and map
  import cats.Monad
  // you need these two extension methods for flatMap and map if you want to use for-comprehensions
  import cats.syntax.flatMap._ // for flatMap
  import cats.syntax.functor._ // for map
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))

  def productWithMonadsForDesugaring[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  // so we've just implemented a Semigroupal using Monads
  // which is the same as:
  /*

   trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]

    def flatMap[A, B](container: M[A])(f: A => M[B]): M[B]

    def map[A, B](container: M[A])(f: A => B): M[B] = flatMap(container)(x => pure(f(x)))

    def product[A, B](fa: M[A], fb: M[B]): M[(A, B)] = flatMap(fa)(a => map(fb)(b => (a, b)))
  }

  and so we easily say that a Monad is a Semigroupal:
  trait MyMonad[M[_]] extends MySemigroupal[M] {
  ...
  }

   */


  // MONADS EXTEND SEMIGROUPALS

  // WHY semiGroupal extends are useful?
  /*
  A Monad product follows the monadic laws (a sequence of flatmap and map), whereas a Semigroupal product does not
  These monad laws impose a sequence of operations, whereas a Semigroupal does not impose any sequence
  a use case for semigroupals is validated
   */
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr] // requires the implicit Semigroup[List[_]] to combine errors
  val invalidsCombination = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "Something else wrong")),
    Validated.invalid(List("This is bad"))
  )

  println(invalidsCombination)

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._ // implicit Monad[Either]
  val eitherSemiGroupal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemiGroupal.product( // in terms of flatMap and map, this is a sequence
    Left(List("Something wrong", "Something else wrong")),
    Left(List("This is bad"))
  )

  // as you can see, this second print lost the last error, because it's a monad and it short-circuits at the first error
  println(eitherCombination)

  // Associativity monadic law : m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
  // this law is not satisfied by Semigroupal

  // TODO 2: define a Semigroupal[List] which does a zip
  val zipListSemigroupal = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }

  println(zipListSemigroupal.product(List(1, 2), List("a", "b")))



}
