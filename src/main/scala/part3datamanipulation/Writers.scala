package part3datamanipulation

import scala.concurrent.Future

object Writers extends App {

  // 1 - define a very dumb writer
  import cats.data.Writer
  // the first one is the log type, the second one is the value type
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)

  // 2 - manipulate the writer with pure FP
  val anIncreasedWriter = aWriter.map(_ + 1) // 46 and the logs are the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something interesting") // the value is the same and the logs are different
  val aWriterWithBoth = aWriter.bimap(_ :+ "found something", _ + 1) // both value and logs are different
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "found something", value + 1)
  } // basically the same as bimap, indeed the bimap uses the mapBoth
  // why the writers are useful? because they allow us to carry logs and values through a chain of operations
  // and so you can define the initial data and then apply the manipulation further in the code in a functional way

  // 3 - reset the logs
  import cats.instances.list._ // a Monoid[List] instance
  // import cats.implicits.catsKernelStdMonoidForList // the same as above
  val aWriterWithNoLog = aWriter.reset

  // 4 - dump the logs
  val desiredValue = aWriter.value
  val logs = aWriter.written
  val (l, v) = aWriter.run

  import cats.instances.vector._ // brings the semigroup for the logs
  val writerA  = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB  = Writer(Vector("Log B1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb
  // ok so we are combining the value, but what about the logs?
  // the logs will be composed as the semigroup of the logs type says
  // so in this case will be concatenated, appended
  println(compositeWriter.run)

  // TODO 1: rewrite a function which "prints" things with writers
  def countAndSay(n: Int) : Unit = {
    if (n <= 0) println("starting")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  println(countAndSay(10))

  def countAndLog(n: Int): Writer[Vector[String], Int] =
    if (n <= 0) Writer(Vector("starting"), 0)
    else {
      countAndLog(n -1).flatMap(_ => Writer(Vector(s"$n"), n))
    }

  println(countAndLog(10).written.mkString("\n"))

  // Benefits of using writers
  // Benefit #1: we work with pure FP

  // TODO 2: rewrite this method with writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def writersSum(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
    else {
      for {
        _ <- Writer(Vector(s"Now at $n"), n)
        lowerSum <- writersSum(n - 1)
        _ <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
      } yield lowerSum + n
      }
  }

  println(naiveSum(5))
  writersSum(5).written.foreach(println)

  // if I'd use the normal naive sum, and launch it twice, I'd get the same logs but the logs would be interleaved
  // like...
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
  Future(naiveSum(10)).foreach(println)
  Future(naiveSum(10)).foreach(println)
  // here we won't understand which log is from which computation

  // but with the writers, we can keep the logs separate!
  val sumFuture1 = Future(writersSum(10))
  val sumFuture2 = Future(writersSum(10))
  val logs1 = sumFuture1.map(_.written) // logs from thread 1
  val logs2 = sumFuture2.map(_.written) // logs from thread 2
  // we can very easily separate the logs from the two computations
  // so the second benefit of using writers is that we can keep logs separate in concurrent programming
  // Benefit #2: we can keep logs separate in concurrent programming



}
