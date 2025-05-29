package part3datamanipulation

object FunctionalState extends App {
  // S is the state type, A is the answer type or the type of the value after each single computation
  type MyState[S, A] = S => (S, A)

  import cats.data.State
  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value

  // why do we need to use State instead of a simple function?
  // because we want to compose multiple state operations

  // state is an abstraction of "iterative" computations
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, now it's $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, now it's $a"

  // this is horrible because it's imperative
  // pure FP with states
  val firstTransformation = State((s: Int) => (s + 1, s"Added 1 to 10, now it's ${s + 1}"))
  val secondTransformation = State((s: Int) => (s * 5, s"Multiplied with 5, now it's ${s * 5}"))
  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap {
    firstResult =>
      secondTransformation.map(secondResult => (firstResult, secondResult))
  }
  println(compositeTransformation.run(10).value)

  val compositeTransformation2 = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  println(compositeTransformation2.run(10).value)

  // why don't chain the functions?
  val func1 = (s: Int) => (s + 1, s"Added 1 to 10, now it's ${s + 1}")
  val func2 = (s: Int) => (s * 5, s"Multiplied with 5, now it's ${s * 5}")
  // more clunky
  val compositeFunc = func1 andThen { case (newState, firstResult) => func2(newState) match {
    case (newestState, secondResult) => ((newState, newestState), (firstResult, secondResult))
  }}
  println(compositeFunc(10)) // this is not the same result as above

  // TODO: an online store
  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State {
    shoppingCart => (ShoppingCart(item :: shoppingCart.items, shoppingCart.total + price), shoppingCart.total + price)
  }

  val shoppingCart = for {
    _ <- addToCart("fender", 500)
    _ <- addToCart("cable", 20)
    total <- addToCart("pick", 2.5)
  } yield total

  println(shoppingCart.run(ShoppingCart(List(), 0)).value)

  // TODO 2: pure mental gymnastics

  // returns a State data structure that, when run, will not change the state but will issue the value f(a)
  def inspect[A, B](f: A => B): State[A,B] = State(a => (a, f(a)))

  // returns a State data structure that, when run, returns the value of that state and makes no changes
  def get[A]: State[A, A] = State(a => (a, a))

  // returns a State data structure that, when run, returns Unit as the value and sets the state to that value
  def set[A](value: A): State[A, Unit] = State(_ => (value, ()))

  // returns a State data structure that, when run, returns Unit as the value and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] = State(a => (f(a), ()))

  // but all these methods are already implemented in the State companion object
  import cats.data.State._

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)
  // it looks like imperative programming but it's not, it's pure FP

}
