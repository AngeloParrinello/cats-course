package part1intro

object TypeClasses extends App {

  case class Person(name: String, age: Int)

  // part1 - type class definition
  // A type class is defined as a trait that operates on a generic type `T`.
  // Here, we define a `JsonSerializer` type class for any type `T`,
  // with a single method `toJson` that converts a value of type `T` to its JSON representation.
  trait JsonSerializer[T] {
    def toJson(value: T): String
  }

  // part2 - create implicit type class instances

  // We define how to serialize different types to JSON format.
  // These are instances of the `JsonSerializer` type class for specific types like `String`, `Int`, and `Person`.
  implicit object StringSerializer extends JsonSerializer[String] {
    override def toJson(value: String): String = "\"" + value + "\""
  }

  implicit object IntSerializer extends JsonSerializer[Int] {
    override def toJson(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JsonSerializer[Person] {
    override def toJson(value: Person): String =
      s"""
         |{"name": ${value.name}, "age": ${value.age}}
         |""".stripMargin.trim
  }

  // part3 - offer some sort of API
  // This function converts a list of values of any type `T` to a JSON string.
  // It takes advantage of the implicit `JsonSerializer` instance for type `T` to convert each element in the list to its JSON representation.
  def convertListToJSON[T](list: List[T])(implicit serializer: JsonSerializer[T]): String =
    list.map(serializer.toJson).mkString("[", ",", "]")


  // part 4 - extending the existing types via extension methods
  // This object defines extension methods to make the `toJson` method more convenient to use.
  // It adds the `toJson` method to any type `T` that has a corresponding implicit `JsonSerializer[T]` available.
  object JsonSyntax {
    implicit class JsonSerializable[T](value: T)(implicit serializer: JsonSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  println(convertListToJSON(List(Person("Alice", 23), Person("Bob", 25))))
  val bob = Person("Bob", 25)
  import JsonSyntax._
  println(bob.toJson)

}
