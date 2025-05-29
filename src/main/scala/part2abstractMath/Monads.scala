package part2abstractMath

object Monads extends App {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  // TODO 1.1 how do you create all combinations of (numbers, chars)?
  val combinationsList: List[(Int, Char)] = numbersList.flatMap(n => charsList.map(c => (n, c)))
  val anotherCombinationsList: List[(Int, Char)] = for {
    n <- numbersList
    c <- charsList
  } yield (n, c)

  // options
  val numbersOption = Option(2)
  val charsOption = Option('d')
  // TODO 1.2 how do you create all combinations of (numbers, chars)?
  val combinationsOption: Option[(Int, Char)] = numbersOption.flatMap(n => charsOption.map(c => (n, c)))
  val anotherCombinationsOption: Option[(Int, Char)] = for {
    n <- numbersOption
    c <- charsOption
  } yield (n, c)

  // futures
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future
  val futureNumbers = Future(42)
  val futureChars = Future('Z')
  // TODO 1.3 how do you create all combinations of (numbers, chars)?
  val combinationsFuture: Future[(Int, Char)] = futureNumbers.flatMap(n => futureChars.map(c => (n, c)))
  val anotherCombinationsFuture: Future[(Int, Char)] = for {
    n <- futureNumbers
    c <- futureChars
  } yield (n, c)

  /*
    Pattern
    - wrapping a value into a monadic (M) value (Option, List, Future)
    - the flatMap mechanism

    MONADS a type class that is higher kinded type (like Functor)
   */

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A] // here we formalize the capability of wrapping a value into a monadic value
    def flatMap[A, B](container: M[A])(f: A => M[B]): M[B]
    // List(1, 2, 3).flatMap(x => List(x, x + 1)) => List(1, 2, 2, 3, 3, 4)
    // Option(2).flatMap(x => Option(x + 1)) => Option(3)
    // Future(42).flatMap(x => Future(x + 1)) => Future(43)
    def map[A, B](container: M[A])(f: A => B): M[B] = flatMap(container)(x => pure(f(x)))
    // the monad type class implement the map method in terms of flatMap and pure
  }

  import cats.Monad
  import cats.instances.option._ // brings the implicit Monad[Option]
  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None) // None

  import cats.instances.list._ // brings the implicit Monad[List]
  val listMonad = Monad[List]
  val aList = listMonad.pure(3) // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1)) // List(3, 4)

  // TODO 2: implement the Monad for Future
  import cats.instances.future._
  val futureMonad = Monad[Future]
  val aFuture = futureMonad.pure(42) // Future(42)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x + 1)) // Future(43)

  // why is this useful since every data type has a flatMap method?
  // it is useful for the general API

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))

  // if we want to support not only the list but also the option or the future, the only way is to copy and paste the same method
  def getPairsOption(numbers: Option[Int], chars: Option[Char]): Option[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairsFuture(numbers: Future[Int], chars: Future[Char]): Future[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))
  // ans so on...

  // but monads help to generalize!
  def getPairs[M[_]: Monad, A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] = monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))
  def anotherGetPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] = monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  println(getPairs(Option(2), Option('d')))
  println(getPairs(List(1, 2, 3), List('a', 'b', 'c')))
  getPairs(Future(42), Future('Z')).foreach(println)
  println(anotherGetPairs(Option(2), Option('d')))
  println(anotherGetPairs(List(1, 2, 3), List('a', 'b', 'c')))
  anotherGetPairs(Future(42), Future('Z')).foreach(println)

  // extension methods for Monad - pure, flatmap
  import cats.syntax.applicative._ // brings the pure method, a sort of weaker monad
  val oneOption = 1.pure[Option] // Option(1) implicit Monad[Option] will be used
  val oneList = 1.pure[List] // List(1) implicit Monad[List] will be used

  import cats.syntax.flatMap._ // brings the flatMap method
  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option]) // Option(2)

  // TODO 3: implement the map method in MyMonad in terms of flatMap and pure (see above the solution..)
  // Monads are also Functors
  // Monad extends Functor
  val oneOptionMapped2 = Monad[Option].map(Option(1))(_ + 1) // Option(2)
  // Monad is a Functor with pure and flatMap
  import cats.syntax.functor._ // brings the map method
  val oneOptionMapped = oneOption.map(_ + 1) // Option(2)

  // the third consequence of monads extend functor is the for comprehensions
  val composedOption = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // TODO 4: implement a shorter version of getPairs using for comprehensions
  // initial version of getPairs with for comprehensions
  def getPairsShorter[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] = {
    ma.flatMap(a => mb.map(b => (a, b))) // here, we are using the extension methods for flatMap and map!!
    // and see that we are no longer using the monad parameter...
  }

  // then...
  def getShorterPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] = for {
    a <- ma
    b <- mb
  } yield (a, b)

  // and then again...
  def getShortestPairs[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] = for {
    a <- ma
    b <- mb
  } yield (a, b)
  // without the implicit Monad[M] parameter, the compiler will inject the Monad[M] instance for the type M
  println(getShortestPairs(Option(2), Option('d')))
  println(getShortestPairs(List(1, 2, 3), List('a', 'b', 'c')))




}
