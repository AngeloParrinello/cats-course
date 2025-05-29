package part2abstractMath

import cats.implicits.catsStdInstancesForFuture
import com.sun.xml.internal.ws.client.AsyncResponseImpl

import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers extends App {

  def sumAllOptions(values: List[Option[Int]]): Int = ???

  import cats.data.OptionT
  import cats.instances.list._ // brings the implicit Monad[List] into scope --> OptionT[Monad[List], A] == List[Option[A]]

  // option transformer: OptionT

  val listOfOptions: List[Option[Int]] = List(Option(1), Option(2), Option(3))
  // but also...
  val listOfOptionsT: OptionT[List, Int] = OptionT(List(Option(1), Option(2), Option(3)))
  val listOfCharOptionsT: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option('c')))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptionsT
    number <- listOfOptionsT
  } yield (number, char)

  // this is basically a convenience API to avoid to unwrap everytime the option
  // inside the list or another structure

  println(listOfTuples.value)

  // either transformer: EitherT
  // if you have a list of Option you'll need a OptionT
  // if you have a list of Either you'll need a EitherT
  import cats.data.EitherT
  val listOfEithers: List[Either[String, Int]] = List(Right(1), Left("Badness"), Right(2))
  val listOfEithersT: EitherT[List, String, Int] = EitherT(List(Left("something wrong"), Right(43), Right(2)))
  implicit val ec: scala.concurrent.ExecutionContext = ExecutionContext.fromExecutorService(java.util.concurrent.Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45))

  /*
  TODO exercise
  We have a multi-machine cluster for your business which will receive a traffic surge following a media appearance
  We measure bandwidth in units. (we don't care what unit is, just that it's an integer)
  We want to allocate TWO of our servers to cope with the traffic spike.
  We know the current capacity for each server and we know we'll hold the traffic if the sum of the bandwidth is > 250.
   */
  val bandwidths = Map(
    "server1" -> 50,
    "server2" -> 300,
    "server3" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T] // wrapper over a Future[Either[String, T]]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT.left(Future(s"Server $server unreachable"))
    case Some(bandwidth) => EitherT.right(Future(bandwidth))
  }

  val futureOfEither2 = EitherT.right(Future(45)) // wrap a Future[Right(45)]

  // true - if the two servers can withstand the surge, false otherwise
  // call getBandwidth twice and check if the sum is > 250
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] =
    for {
      b1 <- getBandwidth(s1)
      b2 <- getBandwidth(s2)
    } yield b1 + b2 > 250
  // by the end of the for comprehension, we have a Future[Either[String, Boolean]]

  // the description of the traffic spike report
  // "s1 and s2 can cope with the incoming traffic"
  // or "servers cannot cope with the incoming traffic"
  // call canWithstandSurge and generate the report (with the transform method)
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] = {
    canWithstandSurge(s1, s2).transform {
      case Left(reason) => Left("Servers cannot cope with the incoming traffic for the following reason: " + reason)
      case Right(false) => Left("Servers cannot cope with the incoming traffic")
      case Right(true) => Right(s"$s1 and $s2 can cope with the incoming traffic")
    }
  // ^^^^^^^^^^^^^^^^ ------ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  // Future[Either[String, Boolean]] => Future[Either[String, String]]
  }

  generateTrafficSpikeReport("server1", "server2").value.foreach(println)
  generateTrafficSpikeReport("server1", "server3").value.foreach(println)


}
