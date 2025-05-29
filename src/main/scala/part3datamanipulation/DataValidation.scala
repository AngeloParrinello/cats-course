package part3datamanipulation

import cats.kernel.Semigroup
object DataValidation extends App {

  // cats has its own way of doing data validation using the Validated data type
  // it is like Either but it can accumulate errors
  import cats.data.Validated
  val aValidValue: Validated[String, Int] = Validated.valid(42) // "valid" is the "right" value similar to Right(42)
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong") // "invalid" is the "left" value similar to Left("Something went wrong")
  // if the condition is true, the value is "valid", otherwise it's "invalid", in this case if true it's 42, otherwise it's "Something wrong"
  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 42, "Something wrong") // same as above

  // TODO: use Either
  /*
  - n must be a prime
  - n must be non-negative
  - n must be even
  - n <= 100
   */
  def isPrime(n: Int): Boolean =
    n > 1 && (2 until n).forall(n % _ != 0)

  def testNumber(n: Int): Either[List[String], Int] = {

    val isPrimeResult = if (isPrime(n)) Right(n) else Left(List("The number must be a prime"))
    val nonNegativeResult = if (n >= 0) Right(n) else Left(List("The number must be non-negative"))
    val evenResult = if (n % 2 == 0) Right(n) else Left(List("The number must be even"))
    val lt100Result = if (n <= 100) Right(n) else Left(List("The number must be less than or equal to 100"))

    for {
      _ <- isPrimeResult
      _ <- nonNegativeResult
      _ <- evenResult
      result <- lt100Result
    } yield result
  }

  // we need a semigroup of list of string and Int
  // approproate combination method for list of strings is concatenation
  import cats.instances.list._ // for Semigroup
  // appropriate combination method for Int
  implicit val combineIntMax: Semigroup[Int] = cats.kernel.Semigroup.instance[Int](Math.max)

  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2 == 0, n, List("The number must be even"))
      .combine(Validated.cond(n >= 0, n, List("The number must be non-negative")))
      .combine(Validated.cond(n <= 100, n, List("The number must be less than or equal to 100")))
      .combine(Validated.cond(isPrime(n), n, List("The number must be a prime")))

  // chain
  // Validated is not a Monad, does not have a flatMap method, but it has a "andThen" method
  // because it can accumulate errors, it can't be a Monad, whereas flatmap would short-circuit at the first error
  aValidValue.andThen(a => Validated.valid(a + 1))

  // test a valid value
  aValidValue.ensure(List("Something went wrong"))(_ % 2 == 0)

  // transform a valid value
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.toUpperCase)
  aValidValue.bimap(_.toUpperCase, _ + 1)

  // interoperate with stdlib
  val eitherToValidate: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidate: Validated[List[String], Int] = Validated.fromOption(None, List("Empty option"))
  val tryToValidate: Validated[Throwable, Int] = Validated.fromTry(scala.util.Try(42))

  // backwards
  aValidValue.toOption
  aValidValue.toEither

  // TODO 2: form validation exercise
  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]

    /*
    - fields are name, email, password
    - the rules are name, email and password MUST be specified
    - name must not be blank
    - email must have "@"
    - password must have at least 10 characters
    - if the form was successfully validated, return a Valid with "Success"
    - if the form was not successfully validated, return a Invalid with the list of errors
     */
    import cats.instances.string._ // for Semigroup[String]
    def validateForm(form: Map[String, String]): FormValidation[String] = {
      val checkFieldsPresence: FormValidation[Unit] = Validated.cond(form.keys.size == 3, (), List("The form must contain exactly 3 fields"))
      val nameIsNotBlank: FormValidation[String] = Validated.cond(form.getOrElse("name", "").nonEmpty, form("name"), List("Name must not be blank"))
      val emailHasAt: FormValidation[String] = Validated.cond(form.getOrElse("email", "").contains("@"), form("email"), List("Email must have @"))
      val passwordLongEnough: FormValidation[String] = Validated.cond(form.getOrElse("password", "").length >= 10, form("password"), List("Password must have at least 10 characters"))

      checkFieldsPresence.andThen(_ => nameIsNotBlank)
        .combine(emailHasAt)
        .combine(passwordLongEnough)
        .map(_ => "Success")
    }

    val form = Map(
      "name" -> "Angelo",
      "email" -> "Angelo@cioa@com",
      "password" -> "1234567890"
    )
    println(validateForm(form))
  }


  import cats.syntax.validated._ // for .valid and .invalid
  val aValidMeaningOfLife: Validated[List[String], Int] = 42.valid[List[String]] // same as Validated.valid[List[String], Int](42)
  val anError: Validated[String, Int] = "Something went wrong".invalid[Int] // same as Validated.invalid[String, Int](List("Something went wrong"))

}
