package part1intro

object Implicits extends App {

  // implicit classes
  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name"
  }

  implicit class ImpersonableString(name: String) {
    // EXTENSION METHOD at the class String
    def greet: String = Person(name).greet
  }

  // explicit way to call the implicit class
  //  val impersonableString = new ImpersonableString("Daniel")
  //  impersonableString.greet

  // implicit way (and preferred one)
  val greeting = "Daniel".greet // new ImpersonableString("Daniel").greet

  // importing implicit conversions in scope
  import scala.concurrent.duration._

  val oneSec = 1.second

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int): Int = x + amount
  implicit val defaultAmount = 10
  val incremented = increment(2) // increment(2)(10)

  def multiply(x: Int)(implicit amount: Int): Int = x * amount
  val times2 = multiply(2) // multiply(2)(10)
  // and is going to take the implicit value from the closest scope

  // more complex example
  trait JsonSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JsonSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  implicit val personSerializer: JsonSerializer[Person] = new JsonSerializer[Person] {
    override def toJson(person: Person): String = s"""{"name": "${person.name}"}"""
  }

  // implicit argument is used to PROVE THE EXISTENCE of a type!!

  def personsJson = listToJson(List(Person("Alice"), Person("Bob")))

  // implicit methods
  implicit def oneArgCaseClassSerializer[T <: Product]: JsonSerializer[T] = new JsonSerializer[T] {
    override def toJson(value: T): String =
      s"""
         |"${value.productElementName(0)}": "${value.productElement(0)}"
         |""".stripMargin
  }

  case class Cat(name: String)
  val catsToJson = listToJson(List(Cat("Garfield"), Cat("Tom")))
  // in background: listToJson(List(Cat("Garfield"), Cat("Tom")))(oneArgCaseClassSerializer[Cat])
  // implicit methods are used to PROVE THE EXISTENCE of a type!!
  // can be used for implicit conversions (but it is not recommended)

  println(oneArgCaseClassSerializer[Cat].toJson(Cat("Garfield")))
  println(oneArgCaseClassSerializer[Person].toJson(Person("Alice")))
  println(catsToJson)

}