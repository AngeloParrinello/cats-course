package part1intro

object CatsIntro {

  // Eq (type class, which allows you to compare two values at compile time and make the code not compilable if they are not of the same time)
  // val aComparison = 2 == "a string" // always false, will trigger a compiler warning

  // part 1 - type class import
  import cats.Eq

  // part 2 - import TC instances for the types you need
  import cats.instances.int._ // brings Eq[Int] into scope

  // part 3 - use the TC API
  val intEquality = Eq[Int]
  val aTypeSafeComparison = intEquality.eqv(2, 3) // false
  // val anUnsafeComparison = intEquality.eqv(2, "a string") // not compile

  // part 4 - use extension methods (if applicable)
  import cats.syntax.eq._
  val anotherTypeSafeComparison = 2 === 3 // false
  val notEqualComparison = 2 =!= 3 // true
  // val invalidComparison = 2 === "a string" // not compile
  // extension methods are ONLY VISIBLE in the presence of the right type class instance in scope

  // compare list
  // part 5 - extending the TC operations to composite types, e.g List
  import cats.instances.list._ // WE BRING Eq[List[Int]]
  val aListComparison = List(2) === List(3) // false


  // we have type class and type instances, but what if the type is not supported?
  // part 6 - create your own type class for a custom type
  case class ToyCar(model: String, price: Double)
  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) =>
    car1.price == car2.price
  }

  val compareTwoToyCars = ToyCar("blue", 19.99) === ToyCar("red", 19.99) // true


}
