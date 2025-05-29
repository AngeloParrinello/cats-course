package part5alien

object InvariantFunctors extends App {

  trait Crypto[A] { self =>
    def encrypt(value: A): String
    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))
      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }
  }

  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)
  def decrypt[A](encrypted: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(encrypted)

  implicit val caesarCypher: Crypto[String] = new Crypto[String] {
    val key = 2
    override def encrypt(value: String): String = value.map(c => (c + key).toChar)
    override def decrypt(encrypted: String): String = encrypted.map(c => (c - key).toChar)
  }
  val encrypted = encrypt("I love cats")
  val decrypted = decrypt[String](encrypted)
  println(encrypted)
  println(decrypted)

  /*
  Problem: what if I want to use the same caesarCypher to encrypt an Int or some other type?
   */
  // let's add to the trait the method imap
  implicit val doubleCrypto: Crypto[Double] = caesarCypher.imap(_.toString, _.toDouble)

  println(decrypt[Double](encrypt(123.4)))

  // todo 1 - support Option[String]
  implicit val optionStringCrypto: Crypto[Option[String]] = caesarCypher.imap(_.getOrElse(""), Option.apply)

  println(encrypt(Option("I love Scala")))
  println(decrypt[Option[String]](encrypted))

  // todo 2 - if you have a crypto[T] => crypto[Option[T]] if you have a Monoid[T] in scope
  import cats.Monoid
  implicit def optionCrypto[A](implicit crypto: Crypto[A], monoid: Monoid[A]): Crypto[Option[A]] =
    crypto.imap(_.getOrElse(monoid.empty), Option(_))

  println(encrypt(Option("I love Scala")))
  println(decrypt[Option[String]](encrypt("I love Scala")))
  import cats.instances.double._
  println(encrypt(Option(Math.PI)))
  println(decrypt[Option[Double]](encrypt(Math.PI)))

  import cats.Invariant
  import cats.Show
  import cats.instances.string._ // for Show[String]
  val showString = Show[String]
  // three argument lists
  // first one the type class instance
  // then how we want to map and back-map the values
  //                                                      forth      back
  val showOptionString: Show[Option[String]] = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))

  import cats.syntax.invariant._ // for imap, extension method
  val showOptionString_v2: Show[Option[String]] = showString.imap(Option(_))(_.getOrElse(""))

  println(showOptionString.show(Some("I love Scala")))
  println(showOptionString.show(None))

  // TODO 3: establish the relation between these trait by implementing the following method in terms of one another
  // who has the stronger methods?
  // MyInvariant is the supertype of MyContravariant and MyFunctor!
  trait MyInvariant[F[_]] { // "Invariant Functor"
    def imap[A, B](fa: F[A])(forth: A => B)(back: B => A): F[B]
  }

  trait MyContravariant[F[_]] extends MyInvariant[F] { // also known as "contravariant functor"
    def contramap[A, B](fa: F[A])(back: B => A): F[B]
    def imap[A, B](fa: F[A])(forth: A => B)(back: B => A): F[B] = contramap(fa)(back)
  }

  trait MyFunctor[F[_]] extends MyInvariant[F] { // also know as "covariant" functor
    def map[A, B](fa: F[A])(forth: A => B): F[B]
    def imap[A, B](fa: F[A])(forth: A => B)(back: B => A): F[B] = map(fa)(forth)

  }

}
