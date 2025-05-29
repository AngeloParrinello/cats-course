package part3datamanipulation

object Readers extends App {

  /*
  Some online store
  - configuration file => initial data structure
  - a DB layer
  - an HTTP layer
  - a business logic layer
   */
  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)
  case class DBConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched" // select * from orders where id = orderId, the implementation is not important
    def getLastOrderId(username: String): Long = 15423 // select max(id) from orders where username = username
  }
  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("Server started") // this would start the actual server, binding to a port, etc
  }

  //bootstrap
  val config = Configuration("admin", "password", "localhost", 8080, 4, "angelo@hotmail.it")
  // readers: we can define a reader to inform the creation of the db connection and the http service based on the configuration
  import cats.data.Reader
  val dbReader: Reader[Configuration, DBConnection] = Reader(conf => DBConnection(conf.dbUsername, conf.dbPassword))
  val dbConn = dbReader.run(config)
  // this reader has a very interesting properties

  // Reader[I, O]
  val myOrderStatusReader: Reader[DBConnection, String] = Reader(dbConnection => dbConnection.getOrderStatus(15423))
  val myOrderStatus = myOrderStatusReader.run(dbConn)

  def getLastOrderStatus(username: String): String = {
    val lastOrderIdReader: Reader[Configuration, String] = dbReader
      .map(dbConn => dbConn.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    // identical to the following
    val usersOrderFor = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

    lastOrderIdReader.run(config)
    usersOrderFor.run(config)
  }

  /*
  Pattern:
  1. You create the initial data structure
  2. You create a reader which specifies how that data structure will be manipulated later
  3. you can then map and flatmap the reader to produce derived information
  4. when you need the final piece of information, you call run on the reader with the initial data structure
   */

  // TODO
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String): String = s"From: $emailReplyTo; To: $address >>> $contents"
  }

  def emailUser(username: String, userEmail: String): String = {
    val emailServiceReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.emailReplyTo))
    val emailUserFor: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      email <- emailServiceReader.map(_.sendEmail(userEmail, s"Your last order has the status: $orderStatus"))
    } yield email

    emailUserFor.run(config)
  }
  // fetch the status of their last order
  // email them with the email service: "Your order has the status: $status"

  // TODO 2: what programming pattern does Reader Monad remind you of?
  // => Dependency Injection


  println(getLastOrderStatus("admin"))
  println(emailUser("admin", "admin@com"))


}
