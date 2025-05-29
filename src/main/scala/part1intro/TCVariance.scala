package part1intro

object TCVariance extends App {

  import cats.Eq
  import cats.instances.int._ // brings Eq[Int] into scope
  import cats.instances.option._ // brings Eq[Option[T]] into scope so also Eq[Option[Int]]
  import cats.syntax.eq._

  val aComparison = Option(2) === Option(3) // false
  // val anInvalidComparison = Some(2) === None // does not compile with Eq[Sone[Int]] not found
  // this related to variance

  // variance
  class Animal
  class Cat extends Animal

  // covariant type: subtyping is propagated to the generic type
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat] // this is possible because Cage is covariant and because Cat is a subtype of Animal, so Cage[Cat] is a subtype of Cage[Animal]

  // contravariant type: subtyping is propagated (backwards) in the opposite direction
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal] // this is possible because Vet is contravariant and because Cat is a subtype of Animal, so Vet[Animal] is a subtype of Vet[Cat]

  // rule of thumb:
  //
  // if a generic type "HAS a T" (which means 'contains' a T) => covariance,
  // if it acts on T, "ACTS on T" => contravariance
  // variance affect how TC instances are being fetched

  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]
  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("making sound")
  makeSound[Animal] // ok - TC instance defined above
  makeSound[Cat] // ok - TC instance for Animal is also valid for Cat because SoundMaker is contravariant
  /*
  How does this work? if we call makeSound[Cat], the compiler will look for an implicit SoundMaker[Cat] instance, but in
  virtue of the contravariance of SoundMaker, it will find the SoundMaker[Animal] instance, which is a valid instance for
   */
  // rule 1: contravariant TCs can use the superclass instances if nothing is available strictly for that type
  // what does this mean? nothing is available for Cat? The compiler will look for something available in a supertype of Cat (in this case Animal)

  // but why this is important?
  // because this has implications for subtypes
  // because if the definition of the type is contravariant, I would be able to define an Eq that compares Some and None
  // let's see an example
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]] // ok - TC instance defined above
  makeSound[Some[Int]] // ok - TC instance for Option[Int] is also valid for Some[Int] because SoundMaker is contravariant


  // covariant TC
  trait AnimalShow[+T] {
    def show: String
  }
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animal"
  }

  implicit object CatShow extends AnimalShow[Cat] {
    override def show: String = "cat"
  }

  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show
  println(organizeShow[Cat]) // ok - CatShow is a subtype of AnimalShow[Cat]
  // println(organizeShow[Animal]) // not ok - ambiguous implicit values
  // this because the compiler sees two different potential implicit values for AnimalShow[Animal] and it does not know which one to pick
  // rule 2: covariant TCs will always use the more specific TC instance for that type
    // but may confuse the compiler if the general TC is also present

  // rule 3: you can't have both benefits
  // Cats uses invariant type classes! In this way we avoid the ambiguity of the compiler
  // when we want to compare two different types, that are related (like Some and None)
  // we have to find a workaround, like:
  Option(2) === Option.empty[Int]

}
