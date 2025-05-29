package part2abstractMath

import cats.implicits.{catsKernelStdGroupForDouble, catsKernelStdMonoidForList}

object Monoids extends App {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // for |+|

  val numbers = (1 to 1000).toList
  // |+| is always associative
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)
  // these two things need to be equal

  println(sumLeft) // 500500
  println(sumRight) // 500500

  // however, let's define a general API
  // def combineFold[T: Semigroup](list: List[T]): T = list.foldLeft(/* what do you put here????*/)(_ |+| _)
  // the answer is that the semigroup is not enough to answer this question! there is no way that a semigroup
  // can provide a default value for the operation, so we need a monoid
  // there would be a starting value if we knew the type T but unfortunately we do not know it in advance

  // the value that we need is called the zero value, or neutral value (the identity) or an empty value
  // An empty value/neutral value/zero value for any kind of type T is the type class called Monoid
  // Monoid is a Semigroup with an empty value
  import cats.Monoid
  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(22, 23)
  val zero = intMonoid.empty

  import cats.instances.string._
  val stringMonoid = Monoid[String]
  val emptyString = stringMonoid.empty // ""
  val combineString = stringMonoid.combine("I understand ", "monoids")

  import cats.instances.option._ // construct an implicit Monoid[Option[Int]]
  val emptyOption = Monoid[Option[Int]].empty // None
  val combineOption = Option(22) |+| Option(20) // Some(42)
  val anotherCombineOption = Option(22) |+| None // Some(22), because we are going to combine a value with a neutral value, a zero value

  // extension methods for Monoids - |+| is the combine method
  // import cats.syntax.monoid._ // either this import or cats.syntax.semigroup._ is needed
  val combinedOptionFancy = Option(3) |+| Option(7)

  // TODO 1: implement a combineFold
  def combineFold[T: Monoid](list: List[T]): T = list.foldLeft(Monoid[T].empty)(_ |+| _) // the Monoid[T] is injected by the compiler
  println(combineFold(numbers))
  println(combineFold(List("I", " love", " monoids")))

  // TODO 2: combine a list of phonebooks as Maps[String, Int]
  // hint: don't construct a monoid - use the default monoid for Map
  val phonebooks = List(
    Map("Alice" -> 235, "Bob" -> 647),
    Map("Charlie" -> 123, "Daniel" -> 789),
    Map("Tina" -> 321)
  )

  import cats.instances.map._
  val bigPhonebook = combineFold(phonebooks)
  println(bigPhonebook)

  // TODO 3 - shopping cart and online stores
  // hint: define your monoid
  case class ShoppingCart(items: List[String], total: Double)
  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance(
    ShoppingCart(List(), 0.0),
    (sc1, sc2) => ShoppingCart(sc1.items |+| sc2.items, sc1.total |+| sc2.total)
  )
  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)
  println(checkout(List(
    ShoppingCart(List("iPhone", "Scala book"), 600),
    ShoppingCart(List("TV"), 400),
    ShoppingCart(List(), 0)
  )))



}
