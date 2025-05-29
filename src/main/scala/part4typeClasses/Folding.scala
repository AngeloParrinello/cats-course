package part4typeClasses

import cats.{Eval, Monoid}

object Folding extends App {

  // Todo implement all in terms of foldLeft & foldRight
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldLeft(List.empty[B])((acc, elem) => acc.appended(f(elem)))
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B])((acc, elem) => acc.concat(f(elem)))
    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldLeft(List.empty[A])((acc, elem) => if (predicate(elem)) acc.appended(elem) else acc)
    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)((x, y) =>  monoid.combine(x, y)) // list.foldLeft(monoid.empty)(monoid.combine)
  }

  val numbers = (1 to 10).toList
  println(ListExercises.map(numbers)(_ * 2))
  println(ListExercises.flatMap(numbers)(x => List(x, x + 1)))
  println(ListExercises.filter(numbers)(_ % 2 == 0))
  import cats.instances.int._ // Monoid[Int]
  println(ListExercises.combineAll(numbers))

  // so, what did we learn so far?
  // that map, flatMap, filter, and combineAll can be implemented in terms of foldLeft and foldRight
  // and that foldLeft and foldRight are the most general and powerful functions in Scala
  // and Cats has a type class for the foldLeft and foldRight operations: Foldable

  import cats.Foldable
  import cats.instances.list._ // implicit Foldable[List]
  val foldable = Foldable[List].foldLeft(List(1,2,3), 0)(_ + _) // 6, same api as foldLeft but in this case we also have a starting value (0)
  import cats.instances.option._ // implicit Foldable[Option]
  val maybeInt = Option(42)
  val foldableMaybe = Foldable[Option].foldLeft(maybeInt, 10)(_ * _) // 420
  // foldRight is slightly different
  // but with Eval, foldRight can be stack safe!!! Regardless the implementation of the data structure container
  val list = List(1,2,3)
  val sumLeft = Foldable[List].foldLeft(list, 0)(_ + _) // 6
  val sumRight: Eval[Int] = Foldable[List].foldRight(list, Eval.now(0)){
    (elem, eval) => eval.map(_ + elem)
  }

  // convenience methods in presence of other type classes instances
  val anotherSum = Foldable[List].combineAll(List(1,2,3)) // implicit Monoid[Int]
  import cats.instances.string._ // Monoid[String]
  val mappedConcat = Foldable[List].foldMap(list)(_.toString) // implicit Monoid[String] => "123"

  // nesting foldables
  val intsNested = List(
    Vector(1,2,3),
    Vector(4,5,6)
  )
  import cats.instances.vector._ // Monoid[Vector[Int]]
  (Foldable[List] compose Foldable[Vector]).combineAll(intsNested) // 21

  // extension methods
  import cats.syntax.foldable._ // extension methods
  val sum3 = list.combineAll // 6, required Monoid[Int], Foldable[List] is in scope
  val mappedConcat2 = list.foldMap(_.toString) // "123", required Monoid[String], Foldable[List] is in scope


}
