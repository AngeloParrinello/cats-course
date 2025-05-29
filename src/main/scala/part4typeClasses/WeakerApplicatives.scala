package part4typeClasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives extends App {

  trait MyApplicative[W[_]] extends MyApply[W] {
    def pure[A](value: A): W[A] // fundamental method of Applicative, all the other stuff is auxiliary
  }

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    // which is an abbreviation for apply
    def ap[A, B](wf: W[A => B])(wa: W[A]): W[B] // fundamental method of Apply

    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    // TODO: implement mapN, given the presence of ap and product
    // i.e mapN((Some(1), Some(2)))(_ + _) => Some(3)
    // i.e. mapN((List(1, 2), List(3, 4)))(_ + _) => List(4, 5, 5, 6)
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val productOfValues: W[(A, B)] = product(tuple._1, tuple._2) // W[(A, B)]
      map(productOfValues) { case (a, b) => f(a, b) }
    }
  }

  // this is what happens in Cats!! There is this structure in Cats
  import cats.Apply
  import cats.instances.option._ // implicit Apply[Option]
  val applyOption = Apply[Option]
  val funcApp = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) // Some(3), btw is almost never used ap directly, alone
  // rather, is used in the context of the product method

  import cats.syntax.apply._ // extension methods from Apply
  val tupleOfOptions = (Option(1), Option(2), Option(3))
  val optionOfTuple = tupleOfOptions.tupled // Some((1, 2, 3))
  val sumOption = tupleOfOptions.mapN(_ + _ + _) // Some(6)

}
