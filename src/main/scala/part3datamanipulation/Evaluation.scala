package part3datamanipulation

import cats.Later

object Evaluation extends App {

  /*
  Cats makes the distinction between:
  - evaluating something eagerly (default evaluation strategy also in scala, call by value)
  - evaluating something lazily (and every time you request it) (call by name)
  - evaluating something lazily and keeping the value so that you don't have to recompute it (MEMOIZING) (call by need)
   */

  import cats.Eval

  val now: Eval[Int] = Eval.now{
    println("Calculating now")
    1000
  }

  // println("First access")
  // println(now.value)

  val redoEval = Eval.always {
    println("Calculating again")
    4234
  }

  // try to comment this line and see what happens
  // println(redoEval.value)
  // println(redoEval.value)

  val delayEval = Eval.later {
    println("Computing later")
    53278
  }
  // println(delayEval.value)
  // println(delayEval.value)

  val composedEvaluation = now.flatMap { firstValue =>
    delayEval.map { secondValue =>
      firstValue + secondValue
    }
  }
  // and if call it...
  //println(composedEvaluation.value)
  //println(composedEvaluation.value)

  val anotherComposedEvaluation = for {
    firstValue <- now
    secondValue <- delayEval
  } yield firstValue + secondValue
  // identical as above

  // todo 1: predict the output
  val evalEx1 = for {
    a <- delayEval // 53278 first and then 53278
    b <- redoEval // 4234 first and then 4234
    c <- now // 100 first and then 1000
    d <- redoEval // 4234 first and then 4234
  } yield a + b + c + d
  // if we dont call the print, the values are not computed, only now
  // if we call once the print,
  // now, later, again, again, sum, again, again, sum

  //println(evalEx1.value)
  //println(evalEx1.value)

  // what's get printed and why?
  //

  // "remember" a computed value
  val dontRecompute = redoEval.memoize // this is the memoization
 ///println(dontRecompute.value)
  //println(dontRecompute.value)

  val tutorial = Eval.always{
    println("Step 1")
    "put the guitar on your lap"
  }.map { step1 =>
    println("Step 2")
    s"$step1 then play the G chord"
  }.memoize // this is the memoization, emember the value up to this point
    .map(step12 => {
      println("Step 3")
      s"$step12 then play the C chord"
    })

  // println(tutorial.value)
  // println(tutorial.value)

  // todo 2: implement defer such that defer(Eval.now) does not run side effects
  // and if the call the value, the side effects are run
  // the eval is passed by name, so it is not evaluated until it is called
  def defer[T](eval: => Eval[T]): Eval[T] = {
    // Eval.later(eval).flatMap(identity)
    // Eval.later(()).flatMap(_ => eval)
    Later().flatMap(_ => eval)
  }

  defer(Eval.now {
    println("Now!")
    42
  })

  defer(Eval.now {
    println("Now!")
    42
  }).value

  // TODO 3: rewrite the method with Eval
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) List()
    else reverseList(list.tail) :+ list.head

  // this is not tail recursive, so it will blow up the stack
  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(List())
    else reverseEval(list.tail).map(_ :+ list.head)

  // println(reverseEval((1 to 10000).toList).value) // it will throw a stack overflow

  // but if we do
  def reverseEvalSafe[T](list: List[T]): Eval[List[T]] = {
    if (list.isEmpty) Eval.now(List())
    else Eval.defer(reverseEvalSafe(list.tail).map(_ :+ list.head)) // is the same as the defer method defined above
  }

  // since we are evaluating lazily, we are not blowing up the stack
  // and moreover the computation is done in a tail rrecursivve manner
  // search for the metjod loop and advance, they stack safe
  println(reverseEvalSafe((1 to 10000).toList).value)

}