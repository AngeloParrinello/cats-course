package part2abstractMath

import scala.annotation.tailrec

object CustomMonads extends App {

  // define pure and flatmap
  // the monad type needs to be type safe

  import cats.Monad
  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    // pure and flatMap are not the only methods in the Monad type class to be implemented
    // starts from an initial value "a" of type A and takes a function that either returns an Either[A, B]
    // but what is it?
    // you start with a value of type A ("a") and you run this function "f" on the value "a" and you obtain an instance
    // of Option[Either[A, B]]. If the Option is empty or is not empty but contains a Left[A], you need to keep
    // running the function "f" on the value "a" until you get a Right[B]. If you get a Right[B], you return a Some[B]
    // and this function must be tail recursive
    // tailrecM does not stack overflow
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(a)) => tailRecM(a)(f)
      case Some(Right(b)) => Some(b)
    }
  }

  // TODO 1: define a monad for the identity type
  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  implicit object IdentityMonad extends Monad[Identity] {

    override def pure[A](x: A): Identity[A] = x

    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(a) => tailRecM(a)(f)
      case Right(b) => b
    }
  }

  // TODO 2: implement a monad for a binary tree
  sealed trait Tree[+A]
  final case class Leaf[+A](value: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  implicit object TreeMonad extends Monad[Tree] {

    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(v) => f(v)
      case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = f(a) match {
      case Leaf(Left(a)) => tailRecM(a)(f)
      case Leaf(Right(b)) => Leaf(b)
      case Branch(l, r) => Branch(
        flatMap(l) {
          case Left(a) => tailRecM(a)(f)
          case Right(b) => Leaf(b)
        },
        flatMap(r) {
          case Left(a) => tailRecM(a)(f)
          case Right(b) => Leaf(b)
        }
      )
    }


  }

  val tree: Tree[Int] = Branch(
    Branch(
      Leaf(1),
      Leaf(2)
    ),
    Leaf(3)
  )

  println(TreeMonad.flatMap(tree)(x => Leaf(x * 10)))





}
