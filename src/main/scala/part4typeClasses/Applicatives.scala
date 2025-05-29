package part4typeClasses

object Applicatives extends App {

  // Functors are the minimum abstraction for a type class that allows us to map over a context
  // Applicatives = Functors + the pure method
  // Applicative extends Functor and adds the pure method
  import cats.Applicative
  import cats.instances.list._ // Applicative[List]
  val listApplicative = Applicative[List]
  val aList: List[Int] = listApplicative.pure(2) // List(2)

  import cats.instances.option._ // implicit Applicative[Option]
  val optionApplicative = Applicative[Option]
  val anOption = optionApplicative.pure(2) // Some(2)

  // pure extension method
  import cats.syntax.applicative._ // import the pure extension method
  val aSweetList: List[Int] = 2.pure[List] // List(2)
  val aSweetOption: Option[Int] = 2.pure[Option] // Some(2)

  // Monads extend Applicatives!
  // Applicative extends Functor
  // Most of the time you'll use Monads
  // Sometimes we use Applicative with Validated
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int] = Validated.valid(43) // "valid" is the "right" value similar to Right(42)
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1) // Valid(List(44))
  val validatedApplicative = Applicative[ErrorsOr]

  // TODO: thought experiment
  // First, try to understand if you can implement the product method with Applicatives --> No, you can't
  def productWithApplicativesFirst[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = ???

  // Second, try now with this helper method
  def ap[W[_], A, B](wa: W[A])(wf: W[A => B]): W[B] = ??? // this is already implemented in cats as the `ap` method
  def productWithApplicativesSecond[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    // input type A
    // output type (A, B)
    // b => (a: A) => (a, b) is a function of type A => (A, B) or in other words B => A => (A, B)
    val functionWrapper: W[A => (A, B)] = applicative.map(wb)(b => (a: A) => (a, b))
    ap(wa)(functionWrapper)
  }

  // the previous exercise was very hard, but it was interesting because it showed us that in a presence of an Applicative
  // you can create a product only if you have this `ap` method
  // but the `ap` method is already implemented in cats as the `ap` method in the Applicative type class
  // so we can just use it
  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(wb)
  }

  // so an Applicative can also be a Semigroupal in a presence of the `ap` method
  // Applicatives has this `ap` method
  // Applicatives can implement product from Semigroupal
  // => Applicatives extends Semigroupal


}
