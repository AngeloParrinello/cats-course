package part5alien

object Kleislis {

  val func1: Int => Option[String] = x => if (x > 0) Some("Positive") else None
  val func2: Int => Option[Int] = x => Some(x * 3)

  // func3 = func1 andThen func2
  val plainFunc1: Int => String = x => if (x > 0) "Positive" else "Negative"
  val plainFunc2: Int => Int = x => x * 3
  val plainFunc3 = plainFunc2 andThen plainFunc1

  // but I cannot do this with Option!!
  // we would need to unwrap the Option and then rewrap it
  // val func3 = func1 andThen func2
  // but in cats...
  import cats.data.Kleisli // the name of a mathematician who worked on the Category Theory, which is the theory behind Cats
  import cats.instances.option._ // implicit FlatMap[Option]
  val func1K: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2K: Kleisli[Option, Int, Int] = Kleisli(func2)
  // val func3K: Kleisli[Option, Int, String]  = func1K andThen func2K

  // convenience
  val multiply = func2K.map(_ * 2)
  //val chain = func3K.flatMap(x => func1K)

  // todo
  import cats.Id
  type InterestingKleisli[A, B] = Kleisli[Id, A, B] // A => Id[B]
  // hint
  val times2 = Kleisli[Id, Int, Int](_ * 2)
  val plus4 = Kleisli[Id, Int, Int](_ + 4)
  val composed = for {
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4

  // what's this pattern remind you of?
  // where we define some operations and then we chain them together?
  // it's a sort of dependency injection
  // and so we've seen the Reader Monad
  // indeed if we replace InterestingKleisli with Reader, we get the Reader Monad!
  // InterestingKleisli[A, B] = Reader[A, B]
  // and if we deep dive in the implementation of Reader, we'll see that it's a Kleisli!
  // Reader = ReaderT[Id, A, B] = Kleisli[Id, A, B]
  import cats.data.Reader



}
