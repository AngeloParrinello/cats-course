package part4typeClasses

import cats.{Applicative, Apply}

object WeakerMonads extends App {

  // the Monad fundamental method is flatMap
  // but it's not specific of Monads ... it's the fundamental method of the trait FlatMap!
  trait MyFlatMap[M[_]] {
    def flatMap[A, B](container: M[A])(f: A => M[B]): M[B]
  }

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    override def map[A, B](container: M[A])(f: A => B): M[B] = flatMap(container)(x => pure(f(x)))
  }

  import cats.Monad // you'll see that Monad extends FlatMap and Applicative

  // but FlatMap extends Apply ??
  // let's suppose thatMyFlatMap extends Apply
  trait MyFlatMap2[M[_]] extends Apply[M] {
    def flatMap[A, B](container: M[A])(f: A => M[B]): M[B]

    // let's try to implement ap, remember that we have the access to Functor so to map
    def ap[A, B](f: M[A => B])(container: M[A]): M[B] =
      flatMap(container)(a => map(f)(ff => ff(a)))
    //        |         |         /  \       \/
    //        |         |    M[A=>B]  A=>B   B
    //        |         |     \_____   _____/
    //       M[A]       A =>        M[B]
}
  // So THERE IS A RELATION BETWEEN FLATMAP AND APPLY

  import cats.FlatMap // you'll see that FlatMap extends Apply
  // almost never used in practice, but it's good to know that there is a relation between FlatMap and Apply
  // but mostly used through its extension method
  import cats.syntax.flatMap._ // flatMap extension methods
  import cats.syntax.functor._ // map extension methods\

  def getPairs[M[_] : FlatMap](numbers: M[Int], chars: M[Char]): M[(Int, Char)] = for {
    n <- numbers
    c <- chars
  } yield (n, c)

  // let's make it more general...
  def getPairs2[M[_] : FlatMap, A, B](numbers: M[A], chars: M[B]): M[(A, B)] = for {
    n <- numbers
    c <- chars
  } yield (n, c)

}
