package part2abstractMath

object UsingMonads extends App {

  import cats.Monad
  import cats.instances.list._ // brings the implicit Monad[List]
  val monadList = Monad[List] // fetch Monad[List] is a monad for List
  val aSimpleList = monadList.pure(2) // List(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1)) // List(2, 3)

  val aManualEither: Either[String, Int] = Right(42)

  // often Either is used for fail-fast computations
  // with a type Alias
  // Either is also a monad!
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._ // brings the implicit Monad[Either]
  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(45) // Right(45) == LoadingOr[Int]
  val aChangedLoading = loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("loading meaning of life"))

  // imagine you have an online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = {
    // not important implementation
    Right(OrderStatus(orderId, "Ready to ship"))
  }

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("not available yet")
    else Right("Amsterdam, NL")

  val orderId = 123L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(trackLocation)
  // use extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._ // for map
  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(trackLocation)

  // or I can do the for comprehension
  import cats.syntax.flatMap._ // for flatMap
  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // TODO the service layer API of a web app
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  // Do not change the code above

  /*
    Requirements:
    - if the host and port are found in the configuration map, then we'll return a M containing a connection with those values
    otherwise the method will fail, according to the logic of the type M
    (for Try it will return a Failure, for Option a None, for Future a failed future, etc.)
    - the issueRequest method returns a M containing the String: "request (payload) has been accepted", if the payload is less
    than 20 characters the method will fail, according to the logic of the type M
   */

  // implement the HttpService for Try, Option, Future, Either

  // Try implementation
  import scala.util.Try
  object TryHttpService extends HttpService[Try] {

    override def getConnection(cfg: Map[String, String]): Try[Connection] =
      for {
        host <- Try(cfg("host"))
        port <- Try(cfg("port"))
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Try[String] =
      Try {
        if (payload.length >= 20) throw new RuntimeException("payload too short")
        s"request ($payload) has been accepted"
      }
  }

  // Option implementation
  object OptionHttpService extends HttpService[Option] {

    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length >= 20) None
      else Some(s"request ($payload) has been accepted")
  }

  val responseOption = OptionHttpService.getConnection(config).flatMap(connection => OptionHttpService.issueRequest(connection, "short payload"))
  val responseOptionFor =
    for {
      connection <- OptionHttpService.getConnection(config)
      response <- OptionHttpService.issueRequest(connection, "short payload")
    } yield response

  println(responseOption)
  println(responseOptionFor)


  // TODO implement another HttpService with LoadingOr or ErrorOr
  // LoadingOr implementation
  object LoadingOrHttpService extends HttpService[LoadingOr] {

    override def getConnection(cfg: Map[String, String]): LoadingOr[Connection] =
      for {
        host <- cfg.get("host").toRight("no host found")
        port <- cfg.get("port").toRight("no port found")
      } yield Connection(host, port)
    override def issueRequest(connection: Connection, payload: String): LoadingOr[String] =
      if (payload.length >= 20) Left("payload too short")
      else Right(s"request ($payload) has been accepted")
  }

  object ErrorOrHttpService extends HttpService[ErrorOr] {

    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      if (cfg.contains("host") && cfg.contains("port")) Right(Connection(cfg("host"), cfg("port")))
      else Left(new RuntimeException("no host or port found"))
    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length >= 20) Left(new RuntimeException("payload too short"))
      else Right(s"request ($payload) has been accepted")
  }

  // now we can define a thing like...
  import cats.instances.option._ // brings the implicit Monad[Option]
  import cats.instances.try_._ // brings the implicit Monad[Try]
  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] =
    for {
      connection <- service.getConnection(config)
      response <- service.issueRequest(connection, payload)
    } yield response

  def getResponseWithFlatmap[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] =
    service.getConnection(config).flatMap(connection => service.issueRequest(connection, payload))

  def getResponseWithExplicitMonad[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] =
    monad.flatMap(service.getConnection(config))(connection => service.issueRequest(connection, payload))

  println(getResponse(OptionHttpService, "short payload"))
  println(getResponse(TryHttpService, "short payload"))

}
