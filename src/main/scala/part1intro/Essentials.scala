package part1intro

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object Essentials {

  // values
  val aBoolean: Boolean = false // immutable

  // expressions are EVALUATED to a value
  val anIfExpression = if(2 > 3) "bigger" else "smaller"

  // instructions vs expressions
  // the instructions are executed (think Java), expressions are evaluated (think Scala)
  val theUnit = println("Hello, Scala!") // Unit = void in other languages // definition of "instructions"

  // OOP
  class Animal
  class Cat extends Animal
  val aCat: Animal = new Cat // polymorphism

  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  // inheritance model: extend only one class, but can inherit from multiple traits
  class Crocodile extends Animal with Carnivore {
    override def eat(animal: Animal): Unit = println("Crunch!")
  }

  // singleton
  object MySingleton // type + the only instance of this type, singleton pattern in one line

  // companions
  object Carnivore // companion object of the trait/class Carnivore
  // scala way to define static methods

  // generics
  class MyList[A] {
    // use the type A
  }

  // method notation
  val three = 1 + 2
  val four = 1.+(3) // equivalent

  // functional programming
  // functions are first-class citizens
  val incrementer: Int => Int = x => x + 1
  val incremented = incrementer(42)

  // higher-order functions: either takes functions as parameters or returns functions
  // map, flatMap, filter in Scala collections
  val processedList = List(1, 2, 3).map(incrementer) // List(2, 3, 4)
  val aLongerList = List(1, 2, 3).flatMap(x => List(x, x + 1)) // List(1, 2, 2, 3, 3, 4)

  // for-comprehensions
  val checkboard = List(1, 2, 3).flatMap(n => List('a', 'b', 'c').map(c => (n, c)))
  val alternativeCheckboard = for {
    n <- List(1, 2, 3)
    c <- List('a', 'b', 'c')
  } yield (n, c)

  // options and try
  val anOption = Option(2) // Some(2)
  val doubledOption = anOption.map(_ * 2) // Some(4)
  val aTry = scala.util.Try {
    throw new RuntimeException
  }
  // val aModifiedTry = aTry.map(_ + 2) // Failure(exception)

  // pattern matching
  val unknown: Any = 2
  val order = unknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  val optionDescription = anOption match {
    case Some(value) => println(s"Got the value: $value")
    case None => println("No value found")
  }

  // Futures
  import scala.concurrent.Future

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(3))

  val aFuture = Future {
    // some expensive computation, runs on another thread
    42
  }

  // wait for completion (async)
  aFuture.onComplete {
    case scala.util.Success(value) => println(s"The async computation was successful: $value")
    case scala.util.Failure(exception) => println(s"The async computation failed: $exception")
  }

  val anotherFuture = aFuture.map(_ + 1) // Future(43)

  // partial functions
  val aPartialFunction : PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 65
    case 5 => 999
  }

  // some more advanced stuff
  trait HigherKindedType[F[_]]
  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }

  val listcChecker = new SequenceChecker[List] {
    override def isSequential: Boolean = true
  }

  def main(args: Array[String]): Unit = {
    println("Hello, Scala!")
  }

}
