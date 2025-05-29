package part5alien

object ContravariantFunctors extends App {
  trait Format[A] {
    def format(value: A): String
  }

  def format[A](value: A)(implicit formatter: Format[A]): String = formatter.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  println(format("Alice"))
  println(format(42))
  println(format(true))

  // problem: given Format[MyType], can we also have a Format[Option[MyType]]? or a Format[List[MyType]]? etc.
  // solution: Contravariant Functors
  implicit def getOptionFormat[A](implicit f: Format[A]): Format[Option[A]] = new Format[Option[A]] {
    override def format(value: Option[A]): String = f.format(value.get)
  }

  // but we have just defined an example for Option but we can define for List, Either, etc.
//  def contramap[A, T](func: A => T)(implicit f: Format[T]): Format[A] = new Format[A] {
//    override def format(value: A): String = f.format(func(value))
//  }

  // but contramap and getOptionFormat are the same thing!!
  // let's define again them...
  implicit def getOptionFormat2[A](implicit f: MyFormat[A]): MyFormat[Option[A]] =
    f.contramap[Option[A]](_.get)

  // let's define again Format because contramap belongs to Format!
  // CONTRAVARIANT TYPE CLASSES
  trait MyFormat[T] { self => // self is a reference to the current instance of MyFormat (i.e. this, in Java)
    def format(value: T): String

    // contramap: the fundamental method of a contravariant type class
    // which is not the same concept as the contravariance of a type!
    def contramap[A](func: A => T): MyFormat[A] = new MyFormat[A] {
      override def format(value: A): String = self.format(func(value))
    }
  }

  // let's remove the contramap method defined above the trait... and test it

  println(format(Option("Alice")))
  println(format(Option(42)))
  // given the fact that we have a format for Option, we can also define an Option[Option[A]] format
  // and so on...
  println(format(Option(Option(42))))
  // and the process is...
  /*
  IntForm
  fo: Format[Option[Int]] = IntFormat.contramap[Option[Int]](_.get) // first get
  fo2: Format[Option[Option[Int]]] = fo.contramap[Option[Option[Int]]](_.get) // second get

  fo2 = IntFormat
            .contramap[Option[Int]](_.get) // first get
            .contramap[Option[Option[Int]]](_.get) // second get

  fo2.format(Option(Option(42))) =
      fo1.format(secondGet(Option(Option(42)))) =
      IntFormat.format(firstGet(secondGet(Option(Option(42)))))

    order of operations: (REVERSE from the written order!)
    - second get
    - first get
    - format of Int

    SO:
    - Map applies transformation in the order they are defined (IN SEQUENCE)
    - Contramap applies transformation in REVERSE order (IN REVERSE SEQUENCE)

    And for this reasons, type classes like this are called CONTRAVARIANT TYPE CLASSES

   */

  // cats has a Contravariant type class
  import cats.Contravariant
  import cats.Show // Show is a type class for showing values as strings, very similar to Format
  import cats.instances.int._ // Show[Int]
  val showInts = Show[Int]
  val showOptionInt: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))
  println(showOptionInt.show(Option(42)))

  import cats.syntax.contravariant._ // for contramap, extention method
  val showOptionInt_v2: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))

}
