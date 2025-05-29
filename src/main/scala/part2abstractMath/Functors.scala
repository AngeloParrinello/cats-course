package part2abstractMath

object Functors extends App {

  // Functors are essentially a type class that allows you to apply a function over a structure, so a map method
  val aModifiedList = List(1, 2, 3).map(_ + 1) // List(2, 3, 4)
  val aModifiedOption = Option(2).map(_ + 1) // Some(3)
  val aModifiedTry = scala.util.Try(42).map(_ + 1) // Success(43)

  // Functors try to generalize the map method
  trait MyFunctor[F[_]] {
    // F in our cases is List, Option, Try
    // whereas the type inside the F is Int, String, etc.
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  // Cats functor
  import cats.Functor
  import cats.instances.list._ // brings Functor[List] into scope
  val listFunctor = Functor[List]
  val incrementedNumbers = listFunctor.map(List(1, 2, 3))(_ + 1) // List(2, 3, 4)

  import cats.instances.option._ // brings Functor[Option] into scope
  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(2))(_ + 1) // Some(3)

  import cats.instances.try_._ // brings Functor[Try] into scope
  val tryFunctor = Functor[scala.util.Try]
  val incrementedTry = tryFunctor.map(scala.util.Try(42))(_ + 1) // Success(43)

  // why do we need to use Functors when almost every data structures implement a map method?
  // functors become useful when we want to generalize a transformation over a structure

  // generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(attempt: scala.util.Try[Int]): scala.util.Try[Int] = attempt.map(_ * 10)

  // generalize
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)
  // def do10x2[F[_]: Functor](container: F[Int]): F[Int] = Functor[F].map(container)(_ * 10) // more concise version

  println(do10x(List(1, 2, 3))) // List(10, 20, 30)
  println(do10x(Option(42))) // Some(420)
  println(do10x(scala.util.Try(42))) // Success(420)

  // TODO 1: define your own functor for a binary tree
  // hint: define an object which extends Functor[Tree]
  trait Tree[+T]
  object Tree {
    // "smart" constructors because they return a general type Tree
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(value) => Leaf(f(value))
      case Branch(value, left, right) => Branch(f(value), map(left)(f), map(right)(f))
    }
  }

  val tree: Tree[Int] = Branch(1, Branch(2, Leaf(3), Leaf(4)), Leaf(5))
  println(TreeFunctor.map(tree)(_ * 2))
  println(do10x(tree)) // without the implicit keyword before TreeFunctor, this would not work, I would have to pass the functor explicitly
  println(do10x(tree).getClass)

  // extension methods
  import cats.syntax.functor._
  val incrementedTree = tree.map(_ + 1)

  // TODO 2: define a short version of do10x
  def do10xShort[F[_]: Functor](container: F[Int]): F[Int] = container.map(_ * 10)
  println(do10xShort(tree))

  // Functors are very useful for general APIs and for abstracting over data structures



}
