package part4typeClasses

import cats.{Foldable, Functor, Monad}

import scala.concurrent.{ExecutionContext, Future}

object Traversing extends App {

  implicit val ec: ExecutionContext = ExecutionContext.global
  val servers: List[String] = List("server-ci.moneyfarm.com", "server-staging.moneyfarm.com", "server-prod.moneyfarm.com")
  def getBandwidth(server: String): Future[Int] = Future(server.length * 80) // not important the implementation

  // I want to get all the bandwidths from all my servers, but I want to do it in parallel, and I want to get the result as a Future[List[Int]]
  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (accumulator, server) =>
    val bandwidth: Future[Int] = getBandwidth(server)
    for {
      accBandwidths <- accumulator
      bandwidth <- bandwidth
    } yield accBandwidths :+ bandwidth
  }
  // this is a solution but it's weird because we need to create Future every time and unwrap and wrap them every time

  // solution 2: use Future.traverse
  val allBandwidths2: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  // and that's it, we get the same result! And under the hood, it's doing the same thing as the foldLeft

  // Future.sequence
  val allBandwidths3: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))
  // servers.map(getBandwidth) returns a List[Future[Int]]
  // and if we sequence it we get a Future[List[Int]]
  // traverse and sequence save us from a lot of pain!

  import cats.syntax.applicative._ // for pure
  import cats.syntax.flatMap._ // for flatMap
  import cats.syntax.functor._ // for map

  // todo 1: implement the listTraverse method, NOT ONLY IN FUTURE, but in any context F[_] that has a Monad
//  def listTraverse[F[_] : Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
//    list.foldLeft(List.empty[B].pure) { // List.empty[B].pure can be replaced with Monad[F].pure(List.empty[B]) or just List.empty[B].pure[F]
//      (accumulator, elem) =>
//        val fb: F[B] = func(elem)
//        for {
//          acc <- accumulator
//          b <- fb
//        } yield acc :+ b
//    }

  // but monad is not necessary because we can do the same with Applicative!
  import cats.Applicative
  import cats.syntax.apply._ // for mapN
  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure) { // List.empty[B].pure can be replaced with Applicative[F].pure(List.empty[B]) or just List.empty[B].pure[F]
      (accumulator, elem) =>
        val fb: F[B] = func(elem)
        (accumulator, fb).mapN(_ :+ _)
    }

  // todo 2: implement the listSequence method
  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse(list)(identity) // x => x

  // todo 3:
  import cats.instances.vector._ // Applicative[Vector]
  println(listSequence(List(Vector(1,2), Vector(3,4)))) // What's the result of this? Vector[List[Int]] - all the possible 2-tuples
  println(listSequence(List(Vector(1,2), Vector(3,4), Vector(5,6)))) // What's the result of this? Vector[List[Int]] - all the possible 2-tuples as before with an additional one (5,6)

  import cats.instances.option._ // Applicative[Option]
  def filterAsOption(list:List[Int])(predicate: Int => Boolean): Option[List[Int]] = {
    // similar to forAll method
    listTraverse(list)(x => Some(x).filter(predicate))
  }

  // todo 4 what's the result of this?
  println(filterAsOption(List(2,4,6))(_ % 2 == 0)) // Some(List(2,4,6))
  println(filterAsOption(List(1,2,3))(_ % 2 == 0)) // Some(List(2))?? NOOO!!! Because the first element is odd, so the result is None, the entire chain fails

  import cats.data.Validated
  import cats.instances.list._ // Semigroup[List] for the combine method
  type ErrorsOr[T] = Validated[List[String], T] // the error type is a List of Strings
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] = {
    listTraverse[ErrorsOr, Int, Int](list) { x =>
      if (predicate(x)) Validated.valid(x)
      else Validated.invalid(List(s"predicate for $x failed"))
    }
  }

  // todo 5 what's the result of this?
  println(filterAsValidated(List(2,4,6))(_ % 2 == 0)) // Valid(List(2,4,6))
  println(filterAsValidated(List(1,2,3))(_ % 2 == 0)) // Invalid(List("predicate for 1 failed", "predicate for 3 failed"))

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_]: Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_]: Applicative, A](container: L[F[A]]): F[L[A]] =
      traverse(container)(identity)

    // TODO 6: implement this using the traverse and/or sequence
    // hint
    type Identity[T] = T

    def map[A, B](container: L[A])(func: A => B): L[B] = {
      //      traverse(container){
      //        a =>
      //          val x = func(a)
      //          val y = Applicative[Identity].pure(x)
      //          y
      //      }
      // or
      // traverse(container)(a => Applicative[Identity].pure(func(a)))
      // or
      // traverse(container)(func(_).pure[Identity])
      // or
      // traverse[Identity, A, B](container)(func)
      // using Identity as the F[_] in traverse, we can map over the container
      // in this way, using a fake wrapper, we have tricked the compiler
      // and the compiler can in somehow, find an applicative for Identity
      // Applicative[Identity] is a valid implicit in scope

      // and in cats there is this thing called Id which is a type alias for the same type
      import cats.Id
      // type Id[T] = T
      // and this Id is a type alias for the Identity type, with the same signature above
      // and if we look into the implementation, Cats create a Bimonad which is also a Monad and so an Applicative
      // and so the compiler can find an Applicative for Id
      // and so we can use the Id type alias instead of the Identity type alias
      traverse[Id, A, B](container)(func)

      // so what's happening here? that mytraverse can now implement map in terms of traverse
      // and so we can say that extends Functor!!!
      // let's change the signature above...
    }
  }

  import cats.Traverse
  import cats.instances.future._ // implicit Applicative[Future]
  val allBandwidths4: Future[List[Int]] = Traverse[List].traverse(servers)(getBandwidth)

  // but it also has some extension methods
  import cats.syntax.traverse._ // extension methods
  val allBandwidths5: Future[List[Int]] = servers.traverse(getBandwidth)

}
