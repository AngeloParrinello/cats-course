package part2abstractMath


object Semigroups extends App {

  // Semigroups COMBINE elements of the same type
  import cats.Semigroup
  import cats.instances.int._
  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46) // what is the natural combination between two integers? My intuition says sum

  println(intCombination)

  import cats.instances.string._
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("I love", " cats") // concatenation

  println(stringCombination)

  // so the type class Semigroup is a type class that allows
  // you to combine elements of the same type
  // for the most common types, you have instances of Semigroup

  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine) // basically list.reduce(_ + _)

  val numbers = (1 to 10).toList
  println(reduceInts(numbers))

  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  val strings = List("I", " love", " cats")
  println(reduceStrings(strings))

  // what if we could define general API?
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  println(reduceThings(numbers)) // compiler injects the implicit Semigroup[Int] instance
  println(reduceThings(strings)) // compiler injects the implicit Semigroup[String] instance

  import cats.instances.option._
  // the compiler will look for an implicit Semigroup[Option[Int]] and it finds it - the combine will produce another Option with the sum of the values inside (if none of them is None)
  val numbersOption: List[Option[Int]] = numbers.map(Option(_))
  println(reduceThings(numbersOption)) // what's happening here? the compiler is looking for an implicit Semigroup[Option[Int]] and it finds it

  val stringsOption: List[Option[String]] = strings.map(Option(_))
  // the compiler will look for an implicit Semigroup[Option[String]] and it finds it - the combine will produce another Option with the concatenation of the values inside (if none of them is None)
  println(reduceThings(stringsOption)) // what's happening here? the compiler is looking for an implicit Semigroup[Option[String]] and it finds it

  // the power of semigroup is this: given a general semigroup instance, I can reduce any list of that type, regardless of the type

  // TODO 1:
  // define a custom semigroup for a custom type
  // hint: use the same patter we used for Eq
  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense]((e1, e2) => Expense(Math.max(e1.id, e2.id), e1.amount + e2.amount))
  //implicit val expenseSemigroup2: Semigroup[Expense] = Semigroup((e1, e2) => Expense(Math.max(e1.id, e2.id), e1.amount + e2.amount)) // this is equivalent to the above

  println(reduceThings(List(Expense(1, 100), Expense(2, 300), Expense(3, 400)))(expenseSemigroup))
  // println(reduceThings(List(Expense(1, 100), Expense(2, 300), Expense(3, 400)))(expenseSemigroup2))

  // extension methods from SemiGroup - |+| is the combine method
  // enhance the existing types with |+|
  import cats.syntax.semigroup._
  val anIntSum = 2 |+| 3 // naturalIntSemigroup.combine(2, 3), requires the presence of the implicit Semigroup[Int]
  val aStringConcat = "we like " |+| "cats" // naturalStringSemigroup.combine("we like ", "cats"), requires the presence of the implicit Semigroup[String]
  val aCombinedExpense = Expense(1, 300) |+| Expense(2, 400) // expenseSemigroup.combine(Expense(1, 300), Expense(2, 400)), requires the presence of the implicit Semigroup[Expense]

  // TODO 2:
  // implement reduceThings2 with the |+| syntax
  def reduceThings2[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(_ |+| _)
  // we can use the type context bound
  // def reduceThings2[T : Semigroup](list: List[T]): T = list.reduce(_ |+| _) // this is equivalent to the above

  println(reduceThings2(numbers))
  println(reduceThings2(strings))
  println(reduceThings2(numbersOption))
  println(reduceThings2(List(Expense(1, 100), Expense(2, 300), Expense(3, 400))))

}
